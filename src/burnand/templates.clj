(ns burnand.templates
  (:use [net.cgrand.enlive-html :as html])
  (:require [burnand.data :as data]
            [burnand.utils :as utils]
            [burnand.settings :as settings]
            [burnand.misc :as misc]))

(defn payment-string [payment]
  (let [prefix (case (:method payment)
                 "espèces" " en "
                 " par ")]
    (str "Acompte versé le " (utils/date-short (:date payment)) prefix (:method payment))))

(html/deftemplate booking-details "public/booking.html" [booking totals]
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
  [:.bookingType :.value :span] (html/content (:bookingType booking))
  [:.bookingType :.value :option] (html/clone-for [i (data/query-setting "bookingTypes")]
                                          (html/do->
                                            (html/content i)
                                            (html/set-attr :value i)))
  [:.comments :.value :span] (html/content (:comments booking))
  [:.comments :.value :textarea] (html/content (:comments booking))
  [:.tax-inc :.value :input] (if (:taxInclusive booking)
                               (html/set-attr :checked "checked")
                               (html/add-class "unchecked"))
  [:#total_services] (html/content (format "%.2f" (:services-total totals))) 
  [:#total_payments] (html/content (format "%.2f" (:payments-total totals))) 
  [:#balance] (html/content (format "%.2f" (:balance totals)))
  [:#for_tax] (html/content (format "%.2f" (:for-tax totals)))
  [:.row.night.new :select :option] (html/clone-for [{id :_id room-name :name}
                                                     (sort #(compare (:order %1) (:order %2)) (data/query-rooms))]
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
                [:.date :span] (html/content (utils/date-short (night :date)))
                [:.date :input] (html/set-attr :value (utils/date-short (night :date)))
                [:.room :span] (html/content (:description night))
                [:.room :select :option] (when
                                           (= (:type night) "room") (html/clone-for
                                                                     [{id :_id room-name :name}
                                                                      (sort #(compare (:order %1) (:order %2)) (data/query-rooms))]
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
  [:.subtotal :.price :span] (html/content (format "%.2f" (float (misc/nights-subtotal booking))))
  [:#taxable] (html/content (str (misc/get-taxable (:products booking))))
  [:#tax_amount] (html/content (format "%.2f" (:tax-rate booking)))
  [:.night.taxtotal :.price :span.val] (html/content (format "%.2f" (misc/tax-total (:products booking) (:tax-rate booking))))
  [:.night.total :.price :span] (html/content (format "%.2f" (misc/nights-total booking)))
  [:.row.new.consumption :.description :option] (html/clone-for
                                                  [consumption
                                                   (conj (reduce #(conj % %2) '({:description "Other:" :price "-1"}) (data/query-consumptions -1))
                                                         {:description "Choose a product" :price ""})]
                                [:option] (html/do->
                                            (html/content (:description consumption))
                                            (html/set-attr :value (:price consumption))))
  [:.row.value.consumption] (html/clone-for [consumption (let [consumptions (filter (comp #{"consumption"} :type) (booking :products))
                                                               sorted-consumptions (sort-by #(vec (map % [:date])) consumptions)]
                                                           sorted-consumptions)]
                                [:.row.value.consumption] (html/set-attr :id (str (:_id consumption)))
                                [:.date :span] (html/content (utils/date-short (:date consumption)))
                                [:.date :input] (html/set-attr :value (utils/date-short (:date consumption)))
                                [:.description :span] (html/content (:description consumption))
                                [:.description :input] (html/set-attr :value (:description consumption))
                                [:.quantity :span] (html/content (str (int (:quantity consumption))))
                                [:.quantity :input] (html/set-attr :value (str (int (:quantity consumption))))
                                [:.each :span] (html/content (format "%.2f" (float (:price consumption))))
                                [:.each :input] (html/set-attr :value (format "%.2f" (float (:price consumption))))
                                [:.price :span] (html/content (format "%.2f" (float (:total consumption)))))
  [:.row.total.consumption :.price :span] (html/content (format "%.2f" (float (misc/consumptions-total booking))))
  [:.buttons_payment :#method :option] (html/clone-for [method settings/payment-methods]
                                [:option] (html/do->
                                            (html/content method)
                                            (html/set-attr :value method)))
  [:.row.value.invoice] (html/clone-for [invoice (filter #(not (:deleted %)) (:invoices booking))]
                            [:.row.value.invoice] (html/set-attr :id (:nr invoice))
                            [:.date.cell] (html/content (utils/date-short (:date invoice)))
                            [:.number.cell :a]
                                        (html/do->
                                          (html/content (:nr invoice))
                                          (html/set-attr :href (str "/invoice/" (:nr invoice))
                                                         :target "_blank"))
                            [:.amount.cell] (html/content (format "%.2f" (float (:amount invoice)))))
  [:.row.value.payment] (html/clone-for [payment (:payments booking)]
                            [:.row.value.payment] (html/set-attr :id (:_id payment))
                            [:.date.cell] (html/content (utils/date-short (:date payment)))
                            [:.method.cell] (html/content  (:method payment))
                            [:.amount.cell] (html/content (format "%.2f" (float (:amount payment)))))
  [:.commission :span] (html/content (format "%.2f" (:commission booking))))

(html/deftemplate create-invoice-html "public/invoice.html"
  [booking invoice-nr date percentage paid products payments totals]
  [:#invoice-info :.page] (html/content "1/1")
  [:#invoice-info :.date] (html/content date)
  [:#invoice-info :.invoice-nr] (html/content invoice-nr)
  [:#name] (html/content (:guestName booking))
  [:#address1] (html/content (:address1 booking))
  [:#address2] (html/content (:address2 booking))
  [:#address3] (html/content (:address3 booking))
  [:#address4] (html/content (:address4 booking))
  [:.row.value] (html/clone-for
                  [product (let [sorted-products
                                 (sort-by #(vec (map % [:date (settings/product-order %)])) products)]
                                           sorted-products)]
                      [:.date] (html/content (utils/date-short (:date product)))
                      [:.description] (html/content (data/get-product-description product))
                      [:.quantity] (html/content (str (:quantity product)))
                      [:.unit] (html/content (format "%.2f" (float (:price product))))
                      [:.price]
                        (html/content (format "%.2f" (float (* (:quantity product)(:price product))))))
  [:.subtotal :.currency] (html/content (format "%.2f" (:products-total totals)))
  [:.row.tax] (when (not (zero? (misc/get-taxable products))) (html/set-attr :class "row tax"))
  [:.row.tax :.taxable] (html/content (str (:taxable totals)))
  [:.row.tax :.tax-rate] (html/content (str (:tax-rate booking)))
  [:.row.tax :.cell.price] (html/content (format "%.2f" (:tax-total totals)))
  [:.row.acompte] (when (< percentage 100) (html/set-attr :class "row acompte"))
  [:.row.paid] (html/clone-for [payment payments]
                  [:.payment] (html/content (payment-string payment))
                  [:.price] (html/content (format "%.2f" (:amount payment))))
  [:.row.amount :.cell.price] (html/content (format "%.2f" (:total-price totals)))
  [:#contact] (html/html-content settings/contact)
  [:#paye] (if paid (html/set-attr :id "paye"))
  [:#bank_info] (html/html-content settings/bank-info))

(html/deftemplate bookings-overview "public/index.html" [bookings]
  [:#tbl_bookings :tr.value]
     (html/clone-for
       [{id :_id
         nm :guestName
         check-in-date :checkInDate
         booking-type :bookingType
         nights :nights :as booking} bookings]
          [:tr.value] (html/set-attr :id (int id))
          [:.check-in-date :span] (html/content (utils/date-short check-in-date))
          [:.name :span] (html/content nm)
          [:.type :span] (html/content booking-type)
          [:.rooms :span] (html/content (data/rooms-per-booking booking))))

(html/deftemplate new-booking "public/new_booking.html" []
  [:#booking_type :option] (html/do-> 
                             (html/after (html/html [:option.value])))
  [:#booking_type :option.value] (html/clone-for [i (data/query-setting "bookingTypes")]
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
                                  max-persons :maxPersons} (sort #(compare (:order %1) (:order %2)) (data/query-rooms))]
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
  [:#check_in_month :option] (html/clone-for [[m n] (map vector (concat (utils/month-list) '("Specify"))
                                                       (concat (utils/month-list-numeric) '("Specify")))]
                                        [:option] (html/do->
                                                    (html/content (str m))
                                                    (html/set-attr :value (str n))))
                                                 
  [:#check_out_month :option] (html/clone-for [[m n] (map vector (concat (utils/month-list) '("Specify"))
                                                       (concat (utils/month-list-numeric) '("Specify")))]
                                        [:option] (html/do->
                                                    (html/content (str m))
                                                    (html/set-attr :value (str n)))))

(html/deftemplate rooms "public/rooms.html" []
  [:#room] (html/content (html/html [:option.room])))

(html/deftemplate tax "public/tax.html" [nights]
  [:table :tr.value] (html/clone-for
                       [night nights]
                       [:tr.value] (html/set-attr :id (:id night))
                       [:.date :span] (html/content (:date night))
                       [:.name :span] (html/content (:guest night))
                       [:.room :span] (html/content (:room night))
                       [:.persons :span] (html/content (str (:persons night)))
                       [:.taxed :input] (html/set-attr :value (:taxed night)
                                                       :name (str (:booking-id night) ":" (:id night)))))

(html/deftemplate fees "public/fees.html" [booking]
  [:#booking-id] (html/content (str (:_id booking)))
  [:#bid] (html/set-attr :value (:_id booking))
  [:#name] (html/content (:guestName booking))
  [:#feetype :option.placeholder] (html/clone-for [feetype settings/fees]
                                                  (html/do-> (html/content feetype)
                                                             (html/set-attr :value feetype)
                                                             (html/remove-class "placeholder")))
  [:.fee] (html/clone-for [fee (:fees booking)]
                          [:.fee] (html/set-attr :id (:_id fee))
                          [:.type] (html/content (:type fee))
                          [:.amount] (html/content (format "%.2f" (:amount fee)))))

