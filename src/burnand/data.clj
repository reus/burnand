(ns burnand.data
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.operators :as mo]
            [monger.query :as mq]
            [clj-time.core :as tm]
            [clojure.java.io :as io]
            [burnand.utils :as utils]
            [burnand.misc :as misc]
            [burnand.settings :as settings])
  (:import org.bson.types.ObjectId))

(defn mongo-connect []
  (let [conn (mg/connect!)
        db  (mg/set-db! (mg/get-db "burnand"))]))

(defn all-bookings []
  (mc/find-maps "bookings"))

(defn remove-products [booking-id product-ids]
  (mc/update "bookings" {:_id booking-id}
             {mo/$pull {:products {:_id {mo/$in product-ids}}}})
  true)

(defn get-booking [id]
  (mc/find-one-as-map "bookings" {:_id id}))

(defn query-rooms []
  (mc/find-maps "rooms"))

(defn keyword-to-room-name [id]
  (let [m (apply merge (map (fn [col] {(keyword (:_id col)) (:name col)}) (query-rooms)))]
    (m id)))

(defn rooms-per-booking [{products :products}]
  (apply str (interpose ", "
                        (map keyword-to-room-name
                             (distinct (map #(keyword (:room %)) (filter #(= (:type %) "room") products)))))))

(defn get-product-description [p]
  (case (:type p)
    "room" (keyword-to-room-name (keyword (:room p)))
    (:description p)))

(defn get-room [room]
  (mc/find-one-as-map "rooms" {:_id room}))

(defn query-bookings
  ([]
   (mq/with-collection "bookings"
     (mq/find {})
     (mq/fields [:_id :guestName :checkInDate :products :payments :bookingType :tax-rate])
     ;(mq/limit 10))))
     (mq/sort (array-map :checkInDate -1))))
  ([start end]
   (mq/with-collection "bookings"
     (mq/find {:checkInDate {mo/$gte start mo/$lte end}})
     (mq/fields [:_id :guestName :checkInDate :products :payments :bookingType :tax-rate])
     (mq/sort (array-map :checkInDate -1)))))

(defn query-setting [key]
  (get (mc/find-by-id "settings" key) "value"))

(defn query-consumptions [o]
  (mq/with-collection "consumptions"
    (mq/fields [:description :price])
    (mq/sort (array-map :description o))))

(defn db-update-product [booking-id product-id new-product]
    (let [result (mc/update "bookings"
                            {:_id booking-id :products._id (org.bson.types.ObjectId. product-id)}
                            {mo/$set {:products.$ new-product}})]
        result))

(defn rate [room]
    (get (get-room room) :rate))

(defn keyword-to-room-name [id]
  (let [m (apply merge (map (fn [col] {(keyword (:_id col)) (:name col)}) (query-rooms)))]
    (m id)))

(defn reset-counter [year]
  (let [new-counter (int (+ (* 10000 year) 1))]
    (mc/update "counters" {:_id "bookingId"} {:seq (+ 1 new-counter)})
    new-counter))

(defn increment-booking-id []
  (let [year (utils/current-year)
        new-id (int (get (mc/find-and-modify "counters" {:_id "bookingId"} {mo/$inc {:seq 1}}) :seq 0))]
      (if (= year (int (/ new-id 10000)))
        new-id
        (reset-counter year))))

(defn db-save-booking [params]
  (let [{check-in :check_in
         check-out :check_out
         guest-name :guest_name
         rooms :rooms
         booking-type :booking_type} params
        ci (utils/parse-date check-in)
        co (utils/parse-date check-out)
        tax-inclusive (if (some #{booking-type} '("booking.com" "airbnb"))
                        true
                        false)
        rooms (filter #(> (read-string (val %)) 0) (:rooms params))
        products (for [date (utils/day-range ci co)
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
                :commission 0.0
                :taxInclusive tax-inclusive
                :tax-rate (query-setting "touristTax")})))

(defn remove-booking [booking-id]
  (mc/remove "bookings" {:_id booking-id}))

(defn edit-products [booking-id products]
  (doseq [product products]
    (case (:type product)
      "room" (mc/update "bookings" {:_id booking-id
                           :products._id (org.bson.types.ObjectId. (:_id product))}
               {mo/$set {:products.$.date (utils/parse-date (:date product))
                         :products.$.room (:room product)
                         :products.$.description (keyword-to-room-name (keyword (:room product)))
                         :products.$.persons (read-string (:persons product))
                         :products.$.taxed (read-string (:taxed product))
                         :products.$.quantity (int 1)
                         :products.$.price (float (read-string (:price product)))
                         :products.$.total (float (read-string (:price product)))}})
      "sup-bed" (mc/update "bookings" {:_id booking-id
                                       :products._id (org.bson.types.ObjectId. (:_id product))}
                           {mo/$set {:products.$.date (utils/parse-date (:date product))
                                     :products.$.description (:description product)
                                     :products.$.quantity (int 1)
                                     :products.$.price (float (read-string (:price product)))
                                     :products.$.total (float (read-string (:price product)))}})
      "consumption" (mc/update "bookings" {:_id booking-id
                                           :products._id (org.bson.types.ObjectId. (:_id product))}
                              {mo/$set {:products.$.date (utils/parse-date (:date product))
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
                            :date (utils/parse-date (:date product))
                            :persons (int (read-string (:persons product)))
                            :taxed (int (read-string (:taxed product)))
                            :price (float (read-string (:price product)))
                            :total (float (read-string (:price product))))
              "sup-bed" (assoc product
                               :_id (org.bson.types.ObjectId.)
                               :quantity (int 1)
                               :date (utils/parse-date (:date product))
                               :price (float (read-string (:price product)))
                               :total (float (read-string (:price product))))
              "consumption" (assoc product
                                   :_id (org.bson.types.ObjectId.)
                                   :quantity (int (read-string (:quantity product)))
                                   :date (utils/parse-date (:date product))
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

(defn update-period [id]
  (let [booking (get-booking id)
        products (:products booking)
        nights (filter (comp #{"room" "sup-bed"} :type) products)
        dates (map #(:date %) nights)
        dates-ordered (sort dates)
        check-in (first dates-ordered)
        check-out (last dates-ordered)]
    (mc/update-by-id "bookings" id {mo/$set {:checkInDate check-in
                                             :checkOutDate check-out}})
  true))

(defn update-general [id data]
  (mc/update-by-id "bookings" id
                   {mo/$set data}))

(defn get-general [booking-id]
  (mc/find-one-as-map "bookings" {:_id booking-id} [:guestName
                                                    :address1
                                                    :address2
                                                    :address3
                                                    :address4
                                                    :bookingType
                                                    :comments]))

(defn add-invoice-to-booking [booking-id invoice]
  (mc/update "bookings"
             {:_id booking-id}
             {mo/$push {:invoices invoice}})
  invoice)

(defn delete-payment [booking-id payment-id]
  (mc/update "bookings" {:_id booking-id}
             {mo/$pull {:payments {:_id payment-id}}}))

(defn delete-invoice [booking-id invoice-nr]
  (io/delete-file (str (:dir settings/invoice-location) invoice-nr))
  (mc/update "bookings"
             {:_id booking-id :invoices.nr invoice-nr}
             {mo/$set {:invoices.$.deleted true}}))

(defn add-payment [b-id date method amount]
  (let [payment {:_id (ObjectId.)
                 :date date
                 :method method
                 :amount amount}]
    (mc/update "bookings"
               {:_id b-id}
               {mo/$push {:payments payment}})
    (assoc payment :date (utils/date-short (:date payment))
                   :_id (.toString (:_id payment))
                   :amount (format "%.2f" (:amount payment)))))

(defn delete-fee [booking-id fee-id]
  (mc/update "bookings" {:_id booking-id}
             {mo/$pull {:fees {:_id fee-id}}}))

(defn add-fee [booking-id feetype amount]
  (let [fee {:_id (ObjectId.)
             :type feetype
             :amount amount}]
    (mc/update "bookings"
               {:_id booking-id}
               {mo/$push {:fees fee}})))


(defn update-bookings-tax []
  (doseq ;[b (query-bookings (tm/date-time 2014 7 1) (tm/date-time 2014 8 31))]
    [b (query-bookings)]
    (let [i (:_id b)
          t (:bookingType b)
          p (:products b)
          d (:checkInDate b)
          taxable (misc/get-taxable p)]
      (if (some #{t} '("airbnb" "booking.com"))
        (mc/update-by-id "bookings" i {mo/$set {:taxInclusive true}})
        (mc/update-by-id "bookings" i {mo/$set {:taxInclusive false}})))))

(defn get-nights []
  (let [bookings (mq/with-collection "bookings"
                   (mq/find {:checkInDate {mo/$gte (tm/date-time 2015 01 01)}})
                   (mq/fields {:guestName 1
                               :products._id 1
                               :products.date 1
                               :products.type 1
                               :products.description 1
                               :products.persons 1
                               :products.taxed 1})
                   (mq/sort (array-map :checkInDate 1)))]
    (mapcat #(for [product (:products %)
                :when (= "room" (:type product))]
            {:id (.toString (:_id product))
             :booking-id (:_id %)
             :date (utils/date-short (:date product))
             :guest (:guestName %)
             :room (:description product)
             :persons (:persons product)
             :taxed (:taxed product)}) bookings)))

(defn update-taxed [booking-id product-id taxed]
  (mc/update "bookings"
             {:_id booking-id :products._id (org.bson.types.ObjectId. product-id)}
             {mo/$set {:products.$.taxed taxed}}))
