(ns burnand.core
  (:use [compojure.core :only (defroutes GET POST)]
        [org.httpkit.server :only [run-server]]
        [ring.middleware.json :only [wrap-json-params]]
        [ring.middleware.keyword-params :only [wrap-keyword-params]]
        [net.cgrand.enlive-html :as html])
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [monger.core :as mg]
            [monger.collection :as mc]
            [clj-time.core :as tm]
            [clj-time.coerce :as tmc]
            [clj-time.format :as tmf]
            [monger.joda-time :as joda]
            [monger.query :as mq]
            [monger.operators :as mo]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clj-pdf.core :as pdf]
            [burnand.settings :as settings]
            [ring.util.response :as resp])
  (:import (java.io FileInputStream)
           org.bson.types.ObjectId))

(def date-format (tmf/formatter "d MMMM yyyy"))

(def date-format-short (tmf/formatter "dd-MM-yyyy"))

(def date-format-js (tmf/formatter "dd/MM/yyyy"))

(def date-string-today (tmf/unparse date-format-short (tm/now)))

(defn date-short [date]
   (tmf/unparse (tmf/with-zone date-format-short
                  (tm/time-zone-for-id "Europe/Paris"))
                date))

(def product-order {:room 0 :sup-bed 1 :consumption 2})

(defn day-range [date1 date2]
  (take-while #(neg? (compare % date2)) (map #(clj-time.core/plus date1 (clj-time.core/days %)) (range))))

(defn currency [double]
  (format "€ %.2f" double))

(defn mongo-connect [location]
  (println "Connecting to database...")
  (mg/connect-via-uri! location))

(defn query-rooms []
  (mc/find-maps "rooms"))

(defn get-room [room]
  (mc/find-one-as-map "rooms" {:_id room}))

(defn rate [room]
    (get (get-room room) :rate))

(defn keyword-to-room-name [id]
  (let [m (apply merge (map (fn [col] {(keyword (:_id col)) (:name col)}) (query-rooms)))]
    (m id)))

(defn query-bookings []
  (let [now (tm/now)
        last-week (tm/minus now (tm/days 8))
        next-weeks (tm/plus now (tm/days 28))]
    (mq/with-collection "bookings"
      ;(mq/find {:checkInDate {mo/$gte last-week mo/$lt next-weeks}})
      (mq/find {})
      (mq/fields [:_id :guestName :checkInDate :products])
      (mq/sort (array-map :checkInDate -1)))))
      ;(mq/limit 10))))

(defn send-bookings-json []
  (let [result (map (fn [{:keys [_id name check_in_date room]}] 
                      {:id (.toString _id) 
                       :name name 
                       :check_in_date (tmc/to-long check_in_date)
                       :room room})(query-bookings))]
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body (json/write-str (vec result))}))

(defn query-setting [key]
  (get (mc/find-by-id "settings" key) "value"))

(defn query-consumptions [o]
  (mq/with-collection "consumptions"
    (mq/fields [:description :price])
    (mq/sort (array-map :description o))))

(defn get-taxable [booking]
  (let [products (:products booking)
        nights (filter (comp #{"room"} :type) products)
        taxable (reduce #(+ % (:taxed %2)) 0 nights)]
     taxable))

(defn month-list []
  (let [now (tm/now)
        custom-formatter (tmf/formatter "MMMM yyyy")]
    (map #(tmf/unparse custom-formatter (tm/plus now (tm/months %))) (range -1 12))))

(defn month-list-numeric []
  (let [now (tm/now)
        custom-formatter (tmf/formatter "MM/yyyy")]
    (map #(tmf/unparse custom-formatter (tm/plus now (tm/months %))) (range -1 12))))

(html/deftemplate new-booking "public/new_booking.html" []
  [:#booking_type :option] (html/do-> 
                             (html/after (html/html [:option.value])))
  [:#booking_type :option.value] (html/clone-for [i (query-setting "bookingTypes")]
                       (html/do-> (html/content (str i))
                             (html/set-attr :value (str i))))
  [:#lst_rooms] (html/append
                  (html/html-snippet
                    (html/sniptest (str "<li class=\"room\">"
                                   "<label></label><select></select></li>")
                              [:li.room]
                              (html/clone-for
                                [{name :name
                                  _id :_id
                                  max-persons :maxPersons} (sort #(compare (:order %1) (:order %2)) (query-rooms))]
                                         [:label] (html/do->
                                                    (html/content name)
                                                    (html/add-class _id)
                                                    (html/set-attr :for _id))
                                         [:select] (html/do->
                                                     (html/append
                                                       {:tag :option
                                                        :attrs nil
                                                        :content "nr of persons"})
                                                     (html/set-attr :id _id :name (str "rooms[" _id "]")))
                                         [:select :option] (html/clone-for
                                                             [i (range (inc max-persons))]
                                                             (html/do->
                                                               (html/content
                                                                 (str i
                                                                      (if (= i 1) " person"
                                                                                  " persons")))
                                                               (html/set-attr :value (str i))))))))
  [:#check_in_month :option] (html/clone-for [[m n] (map vector (concat (month-list) '("Later"))
                                                       (concat (month-list-numeric) '("Later")))]
                                        [:option] (html/content (str m))
                                        [:option] (html/set-attr :value (str n)))

  [:#check_out_month :option] (html/clone-for [[m n] (map vector (concat (month-list) '("Later"))
                                                       (concat (month-list-numeric) '("Later")))]
                                        [:option] (html/content (str m))
                                        [:option] (html/set-attr :value (str n))))

(html/deftemplate rooms "public/rooms.html" []
  [:#room] (html/content (html/html [:option.room])))

(defn increment-booking-id []
  (let [new-id (get (mc/find-and-modify "counters" {:_id "bookingId"} {mo/$inc {:seq 1}}) :seq 0)]
      (int new-id)))

(defn db-update-product [booking-id product-id new-product]
    (let [result (monger.collection/update "bookings"
                                           {:_id booking-id :products._id (org.bson.types.ObjectId. product-id)}
                                           {monger.operators/$set {:products.$ new-product}})]
        result))

(defn db-add-invoice [booking-id invoice]
  nil)

(defn db-save-booking [params]
  (let [{check-in-date :check_in_date
         check-in-month :check_in_month
         check-out-date :check_out_date
         check-out-month :check_out_month
         guest-name :guest_name
         rooms :rooms
         booking-type :booking_type} params
        check-in (str check-in-date "/" check-in-month)
        ci (tmf/parse date-format-js check-in)
        check-out (str check-out-date "/" check-out-month)
        co (tmf/parse date-format-js check-out)
        rooms (filter #(> (read-string (val %)) 0) (:rooms params))
        products (for [date (day-range ci co)
                       [room nr] rooms]
                   {:_id (ObjectId.)
                    :date date
                    :type :room
                    :room room
                    :description (keyword-to-room-name (keyword room))
                    :persons (int (read-string nr))
                    :taxed (int 0)
                    :quantity (int 1)
                    :price (rate room)
                    :total (rate room)})]
    (mc/insert "bookings"
               {:_id (increment-booking-id)
                :guestName guest-name
                :checkInDate ci
                :checkOutDate co
                :bookingType booking-type
                :products products
                :invoices []
                :payments []
                :tax-rate (query-setting "touristTax")})))

(defn ring-save-booking [params]
  (db-save-booking params)
  (resp/redirect "/"))

(defn nights-subtotal [{products :products :as booking}]
  (let [nights (filter (comp #{"room" "sup-bed"} :type) products)
        total (reduce #(+ % (* (:quantity %2) (:price %2))) 0 nights)]
    total))

(defn tax-total [booking]
  (let [tax-rate (:tax-rate booking)
        taxable (get-taxable booking)
        total (* tax-rate taxable)]
    total))

(defn nights-total [{products :products :as booking}]
  (let [subtotal (nights-subtotal booking)
        tx (tax-total booking)
        total (+ subtotal tx)]
    total))

(defn consumptions-total [{products :products}]
  (let [consumptions (filter (comp #{"consumption"} :type) products)
        total (reduce #(+ % (* (:quantity %2) (:price %2))) 0 consumptions)]
    total))

(defn payments-total [{payments :payments}]
  (reduce #(+ (:amount %2) %)  0.0 payments))

(defn products-total [{products :products :as booking}]
  (let [total (reduce #(+ % (* (:quantity %2) (:price %2))) 0.0 products)]
    total))

(defn total-price [booking]
  (- (+ (products-total booking) (tax-total booking)) (payments-total booking)))

(defn keyword-to-room-name [id]
  (let [m (apply merge (map (fn [col] {(keyword (:_id col)) (:name col)}) (query-rooms)))]
    (m id)))

(defn rooms-per-booking [{products :products}]
  (apply str (interpose ", " (map keyword-to-room-name (distinct (map #(keyword (:room %)) (filter #(= (:type %) "room") products)))))))

(html/deftemplate ring-bookings-overview "public/index.html" []
  [:#tbl_bookings :tr.value] (html/clone-for [{id :_id nm :guestName check-in-date :checkInDate nights :nights :as booking}
                                              (query-bookings)]
                                      [:tr.value] (html/set-attr :id (int id))
                                      [:.name] (html/content nm)
                                      [:.check-in-date] (html/content (tmf/unparse date-format check-in-date))
                                      [:.rooms] (html/content (rooms-per-booking booking))))

(html/deftemplate ring-booking-details "public/booking.html" [booking]
  [:#name] (html/content (booking :guestName))
  [:#booking_id] (html/content (str (int (booking :_id))))
  [:.guestName :.value :span] (html/content (booking :guestName))
  [:.guestName :.value :input] (html/set-attr :value (booking :guestName))
  [:.address1 :.value :span] (html/content (booking :address1))
  [:.address1 :.value :input] (html/set-attr :value (booking :address1))
  [:.address2 :.value :span] (html/content (booking :address2))
  [:.address2 :.value :input] (html/set-attr :value (booking :address2))
  [:.address3 :.value :span] (html/content (booking :address3))
  [:.address3 :.value :input] (html/set-attr :value (booking :address3))
  [:.address4 :.value :span] (html/content (booking :address4))
  [:.address4 :.value :input] (html/set-attr :value (booking :address4))
  [:.row.night.new :select :option] (html/clone-for [{id :_id room-name :name}
                                                     (sort #(compare (:order %1) (:order %2)) (query-rooms))]
                                                    (html/do->
                                                      (html/content room-name)
                                                      (html/set-attr :value id)
                                                      (html/add-class "not-selected")))
  [:.row.night.value] (html/clone-for [night (let [nights (filter (comp #{"room" "sup-bed"} :type) (booking :products))
                                                   sorted-nights (sort-by #(vec (map % [:date :type])) nights)]
                                               sorted-nights)]
                [:.row.night] (html/do->
                                  (html/set-attr :id (str (night :_id)))
                                  (if (= (:type night) "room")
                                    (html/add-class (night :room))
                                    (html/set-attr :class "row value bed")))
                [:.date :span] (html/content (date-short (night :date)))
                [:.date :input] (html/set-attr :value (date-short (night :date)))
                [:.room :span] (html/content (:description night))
                [:.room :select :option] (when
                                           (= (:type night) "room") (html/clone-for [{id :_id room-name :name}
                                                                                     (sort #(compare (:order %1) (:order %2)) (query-rooms))]
                                                                                    (html/do->
                                                                                      (html/content room-name)
                                                                                      (html/set-attr :value id)
                                                                                      (if (= id (night :room))
                                                                                        (html/do->
                                                                                          (html/set-attr :selected "selected")
                                                                                          (html/add-class "selected"))
                                                                                        (html/add-class "not-selected")))))
                [:.room :input] (when (= (:type night) "sup-bed") (html/set-attr :value (:description night)))
                [:.nr-of-persons :span] (when (= (:type night) "room") (html/content (str (int (night :persons)))))
                [:.nr-of-persons :input] (when (= (:type night) "room") (html/set-attr :value (str (int (night :persons)))))
                [:.tax :span] (when (= (:type night) "room") (html/content (str (int (night :taxed)))))
                [:.tax :input] (when (= (:type night) "room") (html/set-attr :value (str (int (night :taxed)))))
                [:.price :span] (html/content (format "%.2f" (float (night :price))))
                [:.price :input] (html/set-attr :value (format "%.2f" (float (night :price)))))
  [:.subtotal :.price :span] (html/content (format "%.2f" (float (nights-subtotal booking))))
  [:#taxable] (html/content (str (get-taxable booking)))
  [:#tax_amount] (html/content (format "%.2f" (:tax-rate booking)))
  [:.night.taxtotal :.price :span.val] (html/content (format "%.2f" (tax-total booking)))
  [:.night.total :.price :span] (html/content (format "%.2f" (nights-total booking)))
  [:.row.new.consumption :.description :option] (html/clone-for
                                                  [consumption
                                                   (conj (reduce #(conj % %2) '({:description "Other:" :price "-1"}) (query-consumptions -1))
                                                         {:description "Choose a product" :price ""})]
                                [:option] (html/do->
                                            (html/content (:description consumption))
                                            (html/set-attr :value (:price consumption))))
  [:.row.value.consumption] (html/clone-for [consumption (let [consumptions (filter (comp #{"consumption"} :type) (booking :products))
                                                               sorted-consumptions (sort-by #(vec (map % [:date])) consumptions)]
                                                           sorted-consumptions)]
                                [:.row.value.consumption] (html/set-attr :id (str (:_id consumption)))
                                [:.date :span] (html/content (date-short (:date consumption)))
                                [:.date :input] (html/set-attr :value (date-short (:date consumption)))
                                [:.description :span] (html/content (:description consumption))
                                [:.description :input] (html/set-attr :value (:description consumption))
                                [:.quantity :span] (html/content (str (int (:quantity consumption))))
                                [:.quantity :input] (html/set-attr :value (str (int (:quantity consumption))))
                                [:.each :span] (html/content (format "%.2f" (float (:price consumption))))
                                [:.each :input] (html/set-attr :value (format "%.2f" (float (:price consumption))))
                                [:.price :span] (html/content (format "%.2f" (float (:total consumption)))))
  [:.row.total.consumption :.price :span] (html/content (format "%.2f" (float (consumptions-total booking)))))

(defn assoc-in-last [m [& ks] v]
  (let [es (reduce #(get %1 %2) m ks)]
    (if (coll? es)
      (let [c (count es)
            kss (conj (vec ks) c)]
        (assoc-in m kss v))
      (assoc-in m ks v))))

(def cmprtr (comparator (fn [x y]
                          (if (> (tmc/to-long (:date x)) (tmc/to-long (:date y)))
                            false
                            (if (< (tmc/to-long (:date x)) (tmc/to-long (:date y)))
                              true
                              (let [type-order {"room" 1 "dinner" 2 "product" 3}]
                                (< (type-order (:type x)) (type-order (:type y)))))))))

(defn get-booking [id]
  (mc/find-one-as-map "bookings" {:_id id}))

(defn remove-products [booking-id product-ids]
  (mc/update "bookings" {:_id booking-id}
             {mo/$pull {:products {:_id {mo/$in product-ids}}}})
  true)

(defn edit-products [booking-id products]
  (doseq [product products]
    (case (:type product)
      "room" (mc/update "bookings" {:_id booking-id
                           :products._id (org.bson.types.ObjectId. (:_id product))}
               {mo/$set {:products.$.date (tmf/parse date-format-short (:date product))
                         :products.$.room (:room product)
                         :products.$.description (keyword-to-room-name (keyword (:room product)))
                         :products.$.persons (read-string (:persons product))
                         :products.$.taxed (read-string (:taxed product))
                         :products.$.quantity (int 1)
                         :products.$.price (float (read-string (:price product)))
                         :products.$.total (float (read-string (:price product)))}})
      "sup-bed" (mc/update "bookings" {:_id booking-id
                                       :products._id (org.bson.types.ObjectId. (:_id product))}
                           {mo/$set {:products.$.date (tmf/parse date-format-short (:date product))
                                     :products.$.description (:description product)
                                     :products.$.quantity (int 1)
                                     :products.$.price (float (read-string (:price product)))
                                     :products.$.total (float (read-string (:price product)))}})
      "consumption" (mc/update "bookings" {:_id booking-id
                                           :products._id (org.bson.types.ObjectId. (:_id product))}
                              {mo/$set {:products.$.date (tmf/parse date-format-short (:date product))
                                        :products.$.description (:description product)
                                        :products.$.quantity (int (read-string (:quantity product)))
                                        :products.$.price (float (read-string (:price product)))
                                        :products.$.total (float (*
                                                           (read-string (:quantity product))
                                                           (read-string (:price product))))}}) 
      :default))
  true)

(defn add-products [booking-id products]
  (doseq [product products]
    (let [n (case (:type product)
              "room" (assoc product
                            :_id (org.bson.types.ObjectId.)
                            :description (keyword-to-room-name (keyword (:room product)))
                            :quantity (int 1)
                            :date (tmf/parse date-format-short (:date product))
                            :persons (int (read-string (:persons product)))
                            :taxed (int (read-string (:taxed product)))
                            :price (float (read-string (:price product)))
                            :total (float (read-string (:price product))))
              "sup-bed" (assoc product
                               :_id (org.bson.types.ObjectId.)
                               :quantity (int 1)
                               :date (tmf/parse date-format-short (:date product))
                               :price (float (read-string (:price product)))
                               :total (float (read-string (:price product))))
              "consumption" (assoc product
                                   :_id (org.bson.types.ObjectId.)
                                   :quantity (int (read-string (:quantity product)))
                                   :date (tmf/parse date-format-short (:date product))
                                   :price (float (read-string (:price product)))
                                   :total (* (read-string (:price product))
                                             (read-string (:quantity product))))
              nil)]
      (when n
        (mc/update "bookings"
                   {:_id booking-id}
                   {mo/$push { :products n }}))))
  true)

(defn get-products [booking-id]
  (let [products (:products (mc/find-one-as-map "bookings" {:_id booking-id} [:products]))
        ;nights (filter (comp #{"room" "sup-bed"} :type) products)
        sorted-products (sort-by #(vec (map % [:date :type])) products)]
    sorted-products))

(defn get-products-formatted [booking-id]
  (let [products (get-products booking-id)]
    (map #(assoc %
                 :_id (.toString (:_id %))
                 :date (date-short (:date %))
                 :price (format "%.2f" (float (:price %)))
                 :total (format "%.2f" (float (:total %)))) products)))

(defn ring-save-products [params]
  (let [data (json/read-str (:data params) :key-fn keyword)
        {booking-id :booking_id} data
        b-id (read-string booking-id)
        {r :remove} data
        _r (map #(org.bson.types.ObjectId. %) r)
        {e :edit} data
        {a :add} data
        body (and
               (remove-products b-id _r)
               (edit-products b-id e)
               (add-products b-id a)
               (json/write-str (get-products-formatted b-id)))]
    {:status 200
     :body body
     :headers {"Content-Type" "application/json"}}))

(defn get-general [booking-id]
  (mc/find-one-as-map "bookings" {:_id booking-id} [:guestName
                                                    :address1
                                                    :address2
                                                    :address3
                                                    :address4]))

(defn ring-save-general [{data :data}]
  (let [d (json/read-str data :key-fn keyword)
        id (read-string (:bookingId d))
        name (:name d)
        address1 (:address1 d)
        address2 (:address2 d)
        address3 (:address3 d)
        address4 (:address4 d)]
    (mc/update-by-id "bookings" id
               {mo/$set {:guestName name
                          :address1 address1
                          :address2 address2
                          :address3 address3
                          :address4 address4}})
    {:status 200
     :body (json/write-str (get-general id))
     :headers {"Content-Type" "application/json"}}))

(defn get-product-description [p]
  (case (:type p)
    "room" (keyword-to-room-name (keyword (:room p)))
    (:description p)))
                         
(html/deftemplate ring-invoice "public/invoice.html" [booking invoice-nr date percentage paid]
  [:#invoice-info :.page] (html/content "1/1")
  [:#invoice-info :.date] (html/content date)
  [:#invoice-info :.invoice-nr] (html/content invoice-nr)
  [:#name] (html/content (:guestName booking))
  [:#address1] (html/content (:address1 booking))
  [:#address2] (html/content (:address2 booking))
  [:#address3] (html/content (:address3 booking))
  [:#address4] (html/content (:address4 booking))
  [:.row.value] (html/clone-for [product (let [products (:products booking)
                                               sorted-products (sort-by #(vec (map % [:date (product-order %)])) products)]
                                           sorted-products)]
                                [:.date] (html/content (date-short (:date product)))
                                [:.description] (html/content (get-product-description product))
                                [:.quantity] (html/content (str (:quantity product)))
                                [:.unit] (html/content (format "%.2f" (float (:price product))))
                                [:.price] (html/content (format "%.2f" (float (* (:quantity product)(:price product))))))
  [:.subtotal :.currency] (html/content (format "%.2f" (float (products-total booking))))
  [:.row.tax] (when (not (zero? (get-taxable booking))) (html/set-attr :class "row tax"))
  [:.row.tax :.taxable] (html/content (str (get-taxable booking)))
  [:.row.tax :.tax-rate] (html/content (format "%.2f" (:tax-rate booking)))
  [:.row.tax :.cell.price] (html/content (format "%.2f" (tax-total booking)))
  [:.row.acompte] (when (< percentage 100) (html/set-attr :class "row acompte"))
  [:.row.paid] (html/clone-for [payment (:payments booking)])
  [:.row.amount :.cell.price] (html/content (format "%.2f" (total-price booking)))
  [:#contact] (html/html-content settings/contact)
  [:#paye] (if paid (html/set-attr :id "paye"))
  [:#bank_info] (html/html-content settings/bank-info))

(defn ring-add-invoice
  ([booking] (ring-add-invoice booking date-string-today 100 false))
  ([booking date] (ring-add-invoice booking date 100 false))
  ([booking date percentage] (ring-add-invoice booking date percentage false))
  ([booking date percentage paid]
   (let [invoices (:invoices booking)
         invoice-count (inc (count invoices))
         invoice-nr (str (:_id booking) (format "%02d" invoice-count))]
     (spit (str "resources/public/invoices/" invoice-nr) (apply str (ring-invoice booking invoice-nr date percentage paid))))))

(defn ring-pdf []
  {:status 200
   :headers {"Content-Type" "application/pdf"}
   :body (FileInputStream. "example.pdf")})

(defroutes routes
  ;(GET "/" [] (resp/file-response "index.html" {:root "resources/public"})) 
  (GET "/" [] (ring-bookings-overview))
  (GET "/bookings" [] (send-bookings-json))
  (GET "/booking" [] (new-booking))
  (GET "/booking/:id" [id] (let [bid (read-string id)
                                 booking (get-booking bid)]
                             (if booking
                               (ring-booking-details booking)
                               {:body (str "Could not find booking with id " id)})))
  (GET "/pdf" [] (ring-pdf))
  (GET "/add-invoice" [] (ring-add-invoice))
  (GET "/_invoice/:id" [id] (let [bid (read-string id)
                                  booking (get-booking bid)]
                              (if booking
                                (ring-invoice booking)
                                {:body (str "Could not find booking with id " id)})))
  (GET "/invoice/:id" [id] (do
                             {:status 200
                              :headers {"charset" "UTF-8" "Content-type" "text/html"}
                              :body (io/file (str "resources/public/invoices/" id))}))
  (GET "/rooms" [] (rooms))
  (POST "/save-products" {params :params} (ring-save-products params))
  (POST "/save-general" {params :params} (ring-save-general params))
  (POST "/new" {params :params} (ring-save-booking params))
  (route/resources "/")
  (route/not-found "<p>Sorry, there's nothing here.</p>"))

(def application (handler/site routes))

(defn run []
  (mongo-connect settings/mongo-uri)
  (run-server application {:port 8090 :join? false}))
