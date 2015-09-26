(ns burnand.core
  (:use [compojure.core :only (defroutes GET POST)]
        [org.httpkit.server :only [run-server]])
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [burnand.data :as data]
            [burnand.misc :as misc]
            [burnand.utils :as utils]
            [burnand.templates :as temp]
            [burnand.settings :as settings]
            [ring.util.response :as resp])
  (:import org.bson.types.ObjectId)
  (:gen-class))

(defn ring-delete-booking [params]
  "Ring handler for deleting a booking. Takes parameters 'id' and 'delete'.
  Redirects to home."
  (let [id (read-string (:id params))
        delete (read-string (:delete params))]
    (when delete (data/remove-booking id)))
  (resp/redirect "/"))

(defn ring-save-booking [params]
  "Ring handler for saving a booking"
  (data/db-save-booking params)
  (resp/redirect "/"))

(defn ring-bookings-overview [params]
  "Ring handler for home page. Returns html with a list of bookings."
  (if (and (:q params) (not (empty? (:q params))))
    (let [query (read-string (:q params))]
      (if (number? query)
        (let [booking (data/get-booking query)]
          (if-not (nil? booking)
            (temp/bookings-overview [booking])
            (temp/bookings-overview [])))
        (temp/bookings-overview (data/query-bookings))))
    (temp/bookings-overview (data/query-bookings))))

(defn get-products-formatted [booking-id]
  "Returns a map of the products of given booking 
  with fields _id, date, price and total formatted."
  (let [products (data/get-products booking-id)]
    (map #(assoc %
                 :_id (.toString (:_id %))
                 :date (utils/date-short (:date %))
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
               (data/remove-products b-id _r)
               (data/edit-products b-id e)
               (data/add-products b-id a)
               (data/update-period b-id)
               (json/write-str (get-products-formatted b-id)))]
    {:status 200
     :body body
     :headers {"Content-Type" "application/json"}}))

(defn ring-save-general [{data :data}]
  (let [d (json/read-str data :key-fn keyword)
        id (read-string (:bookingId d))
        d2 (dissoc d :bookingId)
        bk (data/update-general id d2)]
    {:status 200
     :body (json/write-str (data/get-general id))
     :headers {"Content-Type" "application/json"}}))

(defn filter-payments [booking payment-ids]
  (let [payments (:payments booking)]
    (filter #(some #{(.toString (:_id %))} payment-ids) payments)))

(defn filter-products [booking product-ids]
  (let [products (:products booking)]
    (filter #(some #{(.toString (:_id %))} product-ids) products)))

(defn add-invoice
  ([booking] (add-invoice booking (utils/today) 100 false (:products booking) (:payments booking)))
  ([booking date] (add-invoice booking date 100 false (:products booking) (:payments booking)))
  ([booking date percentage] (add-invoice booking date percentage false (:products booking) (:payments booking)))
  ([booking date percentage paid] (add-invoice booking date percentage paid (:products booking) (:payments booking)))
  ([booking date percentage paid products] (add-invoice booking date percentage paid products (:payments booking)))
  ([booking date percentage paid products payments]
   (let [invoices (:invoices booking)
         invoice-count (inc (count invoices))
         invoice-nr (str (:_id booking) (format "%02d" invoice-count))
         url (str (:url-prefix settings/invoice-location) invoice-nr)
         totals (misc/invoice-info products payments (:tax-rate booking))]
     (spit (str (:dir settings/invoice-location) invoice-nr)
           (apply str (temp/create-invoice-html booking
                                           invoice-nr
                                           (utils/date-short date)
                                           percentage
                                           paid
                                           products
                                           payments
                                           totals)))
     (data/add-invoice-to-booking (:_id booking) {:date date
                                             :nr invoice-nr
                                             :url url
                                             :deleted false
                                             :amount (:total-price totals)})
     {:date (utils/date-short date)
      :nr invoice-nr
      :url url
      :amount (format "%.2f" (:total-price totals))})))

(defn ring-delete-payment [params]
  (let [data (json/read-str (:data params) :key-fn keyword)
        {booking-id :bookingId} data
        b-id (read-string booking-id)
        {payment-id :paymentId} data
        p-id (ObjectId. payment-id)]
    (data/delete-payment b-id p-id)
    {:status 200
     :body (json/write-str {:success true})
     :headers {"Content-Type" "application/json"}}))

(defn ring-delete-invoice [params]
  (let [data (json/read-str (:data params) :key-fn keyword)
        {booking-id :bookingId} data
        b-id (read-string booking-id)
        {invoice-nr :invoiceNr} data]
    (data/delete-invoice b-id invoice-nr)
    {:status 200
     :body (json/write-str {:success true})
     :headers {"Content-Type" "application/json"}}))

(defn ring-add-payment [params]
  (let [data (json/read-str (:data params) :key-fn keyword)
        {booking-id :bookingId} data
        b-id (read-string booking-id)
        {datestring :date} data
        date (utils/parse-date datestring)
        {method :method} data
        {amnt :amount} data
        amount (float (read-string amnt))
        payment (data/add-payment b-id date method amount)]
    {:status 200
     :body (json/write-str payment)
     :headers {"Content-Type" "application/json"}}))

(defn ring-add-invoice [params]
  (let [data (json/read-str (:data params) :key-fn keyword)
        {booking-id :bookingId} data
        {datestring :date} data
        date (utils/parse-date datestring)
        b-id (read-string booking-id)
        booking (data/get-booking b-id)
        {product-ids :products} data
        products (filter-products booking product-ids)
        {payment-ids :payments} data
        payments (filter-payments booking payment-ids)
        {p :paid} data
        paid (read-string p)
        {perc :percentage} data
        percentage (read-string perc)
        invoice (add-invoice booking date percentage paid products payments)]
    {:status 200
     :body (json/write-str invoice)
     :headers {"Content-Type" "application/json"}}))

(defn ring-add-fee [])

(defn ring-process-fee [params]
  (println params)
  (if (= "remove" (:action params))
    (data/delete-fee (read-string (:booking-id params))
                     (org.bson.types.ObjectId. (:id params)))
    (if (= "add" (:action params))
      (data/add-fee (read-string (:booking-id params))
                    (:feetype params)
                    (float (read-string (:amount params))))))
  (resp/redirect (str "/fees/" (:booking-id params))))

(defn csv-format [booking]
  [(utils/date-short (:checkInDate booking))
   (:_id booking)
   (:guestName booking)
   (:bookingType booking)
   (if-not (some #{(:bookingType booking)} '("airbnb" "booking.com"))
     (:payments-total (misc/booking-totals booking))
     0.0)
   (if (some #{(:bookingType booking)} '("airbnb"))
     (:payments-total (misc/booking-totals booking))
     0.0)
   (if (some #{(:bookingType booking)} '("booking.com"))
     (:payments-total (misc/booking-totals booking))
     0.0)])
     
(defn create-csv-file [bookings filename]
  (let [b (map csv-format bookings)]
    (with-open [out-file (io/writer filename)]
      (csv/write-csv out-file b))))

(defn tax-csv [filename]
  (let [bookings (data/query-bookings)]
    (create-csv-file bookings filename)))

(defn ring-save-tax [{params :params}]
  (doseq [p params]
    (let [[ids n] p
          [booking-id product-id] (clojure.string/split ids #":")
          result (data/update-taxed (read-string booking-id)
                                    (str product-id)
                                    (read-string n))]
      result))
  {:status 200
   :content "<html></html>"
   :headers {"Content-Type" "text/html"}})


(defroutes routes
  ;(GET "/" [] (resp/file-response "index.html" {:root "resources/public"})) 
  (GET "/" {params :params} (ring-bookings-overview params))
  (GET "/booking" [] (temp/new-booking))
  (GET "/booking/:id" [id] (let [bid (read-string id)
                                 booking (data/get-booking bid)]
                             (if booking
                               (let [totals (misc/booking-totals booking)]
                                 (temp/booking-details booking totals))
                               {:body (str "Could not find booking with id " id)})))
  (POST "/add-invoice" {params :params} (ring-add-invoice params))
  (POST "/add-payment" {params :params} (ring-add-payment params))
  (POST "/delete-invoice" {params :params} (ring-delete-invoice params))
  (POST "/delete-payment" {params :params} (ring-delete-payment params))
  (GET "/invoice/:id" [id] {:status 200
                            :headers {"charset" "UTF-8" "Content-type" "text/html"}
                            :body (io/file (str (:dir settings/invoice-location) id))})
  (GET "/invoice_/:id" [id] {:status 200
                            :headers {"charset" "UTF-8" "Content-type" "text/html"}
                            :body (io/file (str (:dir settings/invoice-location) id))})
  (GET "/rooms" [] (temp/rooms))
  (GET "/tax" [] (temp/tax (data/get-nights)))
  (POST "/tax" request (ring-save-tax request))
  (GET "/fees/:id" [id] (temp/fees (data/get-booking (read-string id))))
  (POST "/fees" {params :params} (ring-process-fee params))
  (POST "/save-products" {params :params} (ring-save-products params))
  (POST "/save-general" {params :params} (ring-save-general params))
  (POST "/new" {params :params} (ring-save-booking params))
  (POST "/delete" {params :params} (ring-delete-booking params))
  (route/resources "/")
  (route/not-found "<p>Sorry, there's nothing here.</p>"))

(def application (handler/site routes))

(defn -main []
  (data/mongo-connect) 
  (run-server application {:port 8090 :join? false}))
