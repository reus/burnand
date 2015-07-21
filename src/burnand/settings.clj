(ns burnand.settings)

(def bank-info (str "<p>Bank information in html.</p>"))

(def contact (str "<p>Contact information in html.</p>"))

(def mongo-uri "mongodb://burnand:burnand@localhost/burnand")

(def local-invoice-location "/Users/ruudreus/Development/burnand/resources/public/invoices/")
(def server-invoice-location "/home/ruudreus/development/burnand/resources/public/invoices/")

(def invoice-location {:url-prefix "/invoice/"
                        :dir server-invoice-location})

(def payment-methods ["espèces"
                      "chèque"
                      "paypal"
                      "virement bancaire"
                      "airbnb"])

(def product-order {:room 0
                    :sup-bed 1
                    :consumption 2})
