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

(defn query-bookings []
  (let [now (tm/now)
        yesterday (tm/minus now (tm/days 2))
        next-weeks (tm/plus now (tm/days 28))]
    (mq/with-collection "bookings"
      (mq/find {:checkInDate {mo/$gte yesterday mo/$lt next-weeks}})
      (mq/fields [:_id :guestName :checkInDate :products])
      (mq/sort (array-map :checkInDate 1)))))
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

(defn get-tax-info [{products :products}]
  (let [nights (filter (comp #{"room"} :type) products)
        taxable (reduce #(+ % (:taxed %2)) 0.0 nights)
        tourist-tax (query-setting "touristTax")
        total (* taxable tourist-tax)]
    {:tourist-tax tourist-tax
     :taxable taxable
     :total total}))


(defn month-list []
  (let [now (tm/now)
        custom-formatter (tmf/formatter "MMMM yyyy")]
    (map #(tmf/unparse custom-formatter (tm/plus now (tm/months %))) (range 12))))

(defn month-list-numeric []
  (let [now (tm/now)
        custom-formatter (tmf/formatter "MM/yyyy")]
    (map #(tmf/unparse custom-formatter (tm/plus now (tm/months %))) (range 12))))

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
  [:#check_in_month] (html/content (html/html [:option]))
  [:#check_in_month :option] (html/clone-for [[m n] (map vector (concat (month-list) '("Later"))
                                                       (concat (month-list-numeric) '("Later")))]
                                        [:option] (html/content (str m))
                                        [:option] (html/set-attr :value (str n)))

  [:#check_out_month] (html/content (html/html [:option]))
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
                    :persons (int (read-string nr))
                    :taxed (int 0)
                    :quantity (int 1)
                    :price (rate room)})]
    (mc/insert "bookings"
               {:_id (increment-booking-id)
                :guestName guest-name
                :checkInDate ci
                :checkOutDate co
                :bookingType booking-type
                :products products
                :invoices []})))

(defn ring-save-booking [params]
  (db-save-booking params)
  (resp/redirect "/"))

 (defn total-price [products]
    (reduce #(+ % (* (:quantity %2) (:price %2))) 0 products))

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

(html/deftemplate ring-booking-details "public/booking.html" [booking tax-info]
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
                [:.room :span] (if (= (:type night) "room")
                                 (html/content (keyword-to-room-name (keyword (night :room))))
                                 (html/content (:description night)))
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
  [:.subtotal :.price :span] (html/content (format "%.2f" (float (total-price (:products booking)))))
  [:#taxable] (html/content (str (int (:taxable tax-info))))
  [:#tax_amount] (html/content (format "%.2f" (:tourist-tax tax-info)))
  [:.taxtotal :.price :span] (html/content (format "%.2f" (:total tax-info)))
  [:.total :.price :span] (html/content (format "%.2f" (+ (float (total-price (:products booking))) (:total tax-info)))))

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

(defn invoice-header [booking date-string show-borders]
  [:table {:border show-borders :cell-border show-borders}
   ["" "" "" "" "" ""]
   [
    [:cell {:colspan 2 :rowspan 2} [:image {:width 123 :height 107}
                                    settings/logo]]
    [:cell {:colspan 2 :align :center}
     [:chunk settings/address]]
    [:cell {:colspan 2 :align :right}
     [:chunk {:size 38 :color [77 77 77]} "\nFacture"]]]
   [[:cell {:colspan 2 :align :center} [:chunk settings/company]]
    [:cell {:align :right}
     [:chunk {:size 11}
      (str "Date:\nN" \u00b0 " de facture:" )]]
    [:cell {:align :right} (str date-string "\n" (int (:_id booking)))]]
   [[:cell {:colspan 6} [:chunk "Facturé à:"]]]
   [[:cell {:colspan 6}
    [:chunk (str (booking :guestName)
                 "\n" (booking :address1)
                 "\n" (booking :address2)
                 "\n" (booking :address3)
                 "\n" (booking :address4))]]]])

(defn invoice-body [products percentage paid show-borders]
  (let [header [[:cell [:paragraph
                        [:chunk "Date"]]]
                [:cell {:align :center} [:paragraph
                        [:chunk "Quantités"] ]]
                [:cell [:paragraph [:chunk "Description"]]]
                ""
                [:cell {:align :center} [:paragraph {:indent 0} [:chunk "Prix à l'unité"]]]
                [:cell {:align :center} [:paragraph {:indent 12} [:chunk "Prix total"]]]]
        cols [[:cell [:paragraph]]
              [:cell [:paragraph]]
              [:cell {:colspan 2} [:paragraph]]
              [:cell [:paragraph]]
              [:cell [:paragraph]]]
        rooms (filter #(= (:type %) "room") products)
        f #(-> %
               (assoc-in-last [0 1] [:chunk (str (date-short (:date %2)) "\n")])
               (assoc-in-last [1 1] [:chunk (str (format "%14d" 1) "\n")])
               (assoc-in-last [2 2] [:chunk {:size 9} (str (keyword-to-room-name (keyword (:room %2)))
                                                 (if (= (:persons %2) 1)
                                                   (" (1 personne)\n")
                                                   (str " (" (int (:persons %2)) " personnes)\n")))])
               (assoc-in-last [3 1] [:chunk {:family :courier}
                                     (str (format "%11s" (currency (:price %2))) "\n")])
               (assoc-in-last [4 1] [:chunk {:family :courier}
                                     (str (format "%11s" (currency (:price %2))) "\n")]))
        product-rows (reduce f cols rooms)
        total (total-price products)
        row-product-totals [[:cell {:colspan 5} [:chunk "Montant total:"]]
                        [:cell [:chunk {:family :courier} (format "%11s" (currency total))]]]
        row-product-pay [[:cell {:colspan 5} [:chunk {:style :bold} "À PAYER"]]
                         [:cell [:chunk {:family :courier :style :bold}
                               (format "%11s" (currency (* (/ percentage 100) total)))]]]
        row-tva-info [[:cell {:colspan 6} [:paragraph "TVA non applicable, article 293 B du CGI."]]]
        row-downpayment (when (< percentage 100)
                          [[:cell
                            {:colspan 5} [:chunk (str "Acompte " percentage "% de " (currency total))]]
                           [:cell [:chunk {:family :courier} (format "%11s" (currency (* (/ percentage 100) total)))]]])
        row-paid (when paid
                   [[:cell
                     {:colspan 5} [:chunk (str "Payé " (get paid :date) " " (get paid :method))]]])
        table [:table
               {:border show-borders 
                :cell-border show-borders}
               header product-rows row-product-totals row-downpayment row-product-pay row-tva-info row-paid]]
    (filter #(not (nil? %)) table)))

(defn invoice-footer [lines-available]
  (let [footer (list [:chunk {:size 10 :align :center} settings/footer-text])
        ]
    (if (not (neg? lines-available))
      (into footer (repeat lines-available [:chunk "\n"])))))

(defn create-invoice
  ([booking] (create-invoice booking false 100 date-string-today))
  ([booking paid] (create-invoice booking paid 100 date-string-today))
  ([booking paid percentage] (create-invoice booking paid percentage date-string-today))
  ([booking paid percentage date-string]
    (let [products (filter #(not (:invoice %)) (sort cmprtr (:products booking)))
          p (count products)
          p (if (< percentage 100) (inc p) p)
          p (if paid (inc (inc p)) p)
          lines 13
          lines-available (- lines p)
          filename (str "resources/public/pdf/facture" (int (:_id booking)) ".pdf")]
      (pdf/pdf [{:title "Facture"
                 :author settings/pdf-author
                 :size :a4
                 :footer false
                 :font {:family :helvetica :size 10}}
                (invoice-header booking date-string false)
                (invoice-body products percentage paid false)
                (invoice-footer lines-available)]
               filename)
      filename)))

(defn ring-pdf []
  {:status 200
   :headers {"Content-Type" "application/pdf"}
   :body (FileInputStream. "example.pdf")})

(defn get-booking [id]
  (mc/find-one-as-map "bookings" {:_id id}))


(defn remove-nights [booking-id product-ids]
  (mc/update "bookings" {:_id booking-id}
             {mo/$pull {:products {:_id {mo/$in product-ids}}}})
  true)

(defn edit-nights [booking-id nights]
  (doseq [night nights]
    (case (:type night)
      "room" (mc/update "bookings" {:_id booking-id
                           :products._id (org.bson.types.ObjectId. (:_id night))}
               {mo/$set {:products.$.date (tmf/parse date-format-short (:date night))
                         :products.$.room (:room night)
                         :products.$.persons (read-string (:persons night))
                         :products.$.taxed (read-string (:taxed night))
                         :products.$.price (float (read-string (:price night)))}})
      "sup-bed" (mc/update "bookings" {:_id booking-id
                                       :products._id (org.bson.types.ObjectId. (:_id night))}
                           {mo/$set {:products.$.date (tmf/parse date-format-short (:date night))
                                     :products.$.description (:description night)
                                     :products.$.price (float (read-string (:price night)))}})
      :default))
  true)

(defn add-nights [booking-id nights]
  (doseq [night nights]
    (case (:type night)
      "room" (let [n (assoc night
                   :_id (org.bson.types.ObjectId.)
                   :quantity (int 1)
                   :type "room"
                   :date (tmf/parse date-format-short (:date night))
                   :persons (int (read-string (:persons night)))
                   :taxed (int (read-string (:taxed night)))
                   :price (float (read-string (:price night))))]
               (mc/update "bookings"
                          {:_id booking-id}
                          {mo/$push { :products n }}))
      "sup-bed" (let [n (assoc night
                   :_id (org.bson.types.ObjectId.)
                   :quantity (int 1)
                   :type "sup-bed"
                   :description (:description night)
                   :date (tmf/parse date-format-short (:date night))
                   :price (float (read-string (:price night))))]
      (mc/update "bookings"
               {:_id booking-id}
               {mo/$push { :products n }}))))
  true)

(defn get-nights [booking-id]
  (let [products (:products (mc/find-one-as-map "bookings" {:_id booking-id} [:products]))
        nights (filter (comp #{"room" "sup-bed"} :type) products)
        sorted-nights (sort-by #(vec (map % [:date :type])) nights)]
    sorted-nights))

(defn get-nights-formatted [booking-id]
  (let [nights (get-nights booking-id)]
    (map #(assoc %
                 :_id (.toString (:_id %))
                 :date (date-short (:date %))
                 :room_full (keyword-to-room-name (keyword (:room %)))
                 :price (format "%.2f" (float (:price %)))) nights)))


(defn my-value-reader [key value]
  (if (= key :iremove)
    (str value)))

(defn ring-save-nights [params]
  (let [data (json/read-str (:data params) :key-fn keyword)
        {booking-id :booking_id} data
        b-id (read-string booking-id)
        {r :remove} data
        _r (map #(org.bson.types.ObjectId. %) r)
        {e :edit} data
        {a :add} data
        body (and
               (remove-nights b-id _r)
               (edit-nights b-id e)
               (add-nights b-id a)
               (json/write-str (get-nights-formatted b-id)))]
    {:status 200
     :body body
     :headers {"Content-Type" "application/json"}}))

(defn get-personal [booking-id]
  (mc/find-one-as-map "bookings" {:_id booking-id} [:guestName
                                                    :address1
                                                    :address2
                                                    :address3
                                                    :address4]))

(defn ring-save-personal [{data :data}]
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
     :body (json/write-str (get-personal id))
     :headers {"Content-Type" "application/json"}}))

(html/deftemplate ring-invoice "public/invoice.html" [])

(html/deftemplate ring-add-invoice "public/invoice.html" [])

(defroutes routes
  ;(GET "/" [] (resp/file-response "index.html" {:root "resources/public"})) 
  (GET "/" [] (ring-bookings-overview))
  (GET "/bookings" [] (send-bookings-json))
  (GET "/booking" [] (new-booking))
  (GET "/booking/:id" [id] (let [bid (read-string id)
                                 booking (get-booking bid)]
                             (if booking
                               (ring-booking-details booking (get-tax-info booking))
                               {:body (str "Could not find booking with id " id)})))
  (GET "/pdf" [] (ring-pdf))
  (GET "/add-invoice" [] (ring-add-invoice))
  (GET "/invoice/:id" [id] (ring-invoice id))
  (GET "/rooms" [] (rooms))
  (POST "/save-nights" {params :params} (ring-save-nights params))
  (POST "/save-personal" {params :params} (ring-save-personal params))
  (POST "/new" {params :params} (ring-save-booking params))
  (route/resources "/")
  (route/not-found "<p>Sorry, there's nothing here.</p>"))

(def application (handler/site routes))

(defn run []
  (mongo-connect settings/mongo-uri)
  (run-server application {:port 8090 :join? false}))
