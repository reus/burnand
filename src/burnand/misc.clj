(ns burnand.misc)

(defn get-taxable [products]
  (let [nights (filter (comp #{"room"} :type) products)
        taxable (reduce #(+ % (:taxed %2)) 0 nights)]
     taxable))

(defn tax-total [products tax-rate]
  (let [taxable (get-taxable products)
        total (* tax-rate taxable)]
    total))

(defn nights-subtotal [{products :products :as booking}]
  (let [nights (filter (comp #{"room" "sup-bed"} :type) products)
        total (reduce #(+ % (* (:quantity %2) (:price %2))) 0 nights)]
    total))

(defn nights-total [{products :products :as booking}]
  (let [subtotal (nights-subtotal booking)
        tx (tax-total products (:tax-rate booking))
        total (+ subtotal tx)]
    total))

(defn consumptions-total [{products :products}]
  (let [consumptions (filter (comp #{"consumption"} :type) products)
        total (reduce #(+ % (* (:quantity %2) (:price %2))) 0 consumptions)]
    total))

(defn payments-total [payments]
  (reduce #(+ (:amount %2) %)  0.0 payments))

(defn products-total [products]
  (let [total (reduce #(+ % (* (:quantity %2) (:price %2))) 0.0 products)]
    total))

(defn total-price [products payments tax-rate]
  (- (+ (products-total products) (tax-total products tax-rate)) (payments-total payments)))

(defn invoice-info [products payments tax-rate]
  {:products-total (products-total products)
   :taxable (get-taxable products)
   :tax-total (tax-total products tax-rate)
   :total-price (total-price products payments tax-rate)})

(defn booking-totals [booking]
  (let [products (:products booking)
        tax-rate (:tax-rate booking)
        payments (:payments booking)
        services-total (+ (tax-total products tax-rate) (products-total products))
        p-total (payments-total payments)
        balance (- services-total p-total)
        balance (if (< -0.001 balance 0.001) 0.0 balance)]
    {:for-tax (products-total products)
     :services-total services-total
     :payments-total p-total
     :balance balance}))
