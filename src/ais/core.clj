(ns ais.core
  (:gen-class)
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as string]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


;; (s/conform even? 1000)

;; (s/valid? even? 10)


;; (def id-regex #"^[0-1]*$")
;; (re-matches id-regex "0")

;; (s/def ::binario
;;   (s/and
;;     string?
;;     #(re-matches id-regex %)
;;     #(not (string/blank? %))))

;; (s/conform ::binario "111010101010101")

;; (s/def ::mensagem
;;   (s/cat :messageId ::binario
;;          :repeatIndicator ::binario))

;; (s/conform ::mensagem ["10000" "1110101"])


;; (s/def ::binario-pequeno
;;   (s/and
;;      ::binario
;;      #(< (Integer/parseInt % 2) 10)))

;; (s/conform ::binario-pequeno "111111")

;; (Integer/parseInt "1111" 2)





(.length "aaaa")

(def exemplo "177KQJ5000G?tO`K>RA1wUbN0TKH")

(println exemplo)
(map int (seq exemplo))

(defn ascii86 [x]
  (cond 
    (<= 32 x 63) x
    (<= 64 x 95) (- x 64)))

(defn decode-payload-char [x]
  (cond
    (<= 48 x 87) (- x 48)
    (<= 96 x 119) (- x 56)))


(defn payload-to-binary-string
  "transforma de string do payload AIS para string de binario"
  [x]
  (string/join
    (map #(format "%06d"
           (Integer/parseInt
            (Integer/toString (decode-payload-char (int %)) 2))) 
      (seq x))))


(payload-to-binary-string exemplo)


;; tamanhos dos parametros da mensagem tipo 1
(def pedacos [6 2 30 4 8 10 1 28 27 12 9 6 2 3 1 19])


(defn regex-split
  ""
  [x]
  (re-pattern
   (string/join 
    (map #(str "(.{" % "})") x))))

;; (regex-split pedacos)                   

;; (map #(Integer/parseInt % 2)
;;      (rest 
;;       (re-matches (regex-split pedacos) 
;;                   (payload-to-binary-string exemplo))))

(def substrings01 
  (rest 
   (re-matches (regex-split pedacos) 
               (payload-to-binary-string exemplo))))





(defn decode-message1
  [x]
  (let [bin-strings-size [6 2 30 4 8 10 1 28 27 12 9 6 2 3 1 19]
        parameters (rest (re-matches (regex-split bin-strings-size)
                                     (payload-to-binary-string x)))
        [message-id repeat-indicator user-id nav-status rate-of-turn
         sog pos longitude latitude cog true-heading timestamp
         special-manoevre spare raim-flag comm-state] parameters]        
   (list (bin-to-dec message-id)
    (bin-to-dec repeat-indicator)
    (bin-to-dec user-id)
    (bin-to-navigational-status nav-status)
    (bin-to-rate-of-turn rate-of-turn)
    (bin-to-sog sog)
    (bin-to-position-accuracy pos)
    (bin-to-longitude longitude)
    (bin-to-latitude latitude)
    (bin-to-cog cog)
    (bin-to-true-heading true-heading)
    (bin-to-timestamp timestamp)
    (bin-to-dec special-manoevre)
    (bin-to-dec spare)
    (bin-to-dec raim-flag)
    (bin-to-dec comm-state))))

(pprint (decode-message1 exemplo))

  ;; (map (fn [x y] ((resolve x) y)) 
  ;;     (decoders-message1) 
  ;;     substrings01)


(foo1 5)

((resolve (nth decoders-message1 3)) (nth substrings01 3))

(bin-to-dec (nth substrings01 0))

(defn bin-to-navigational-status
  [x]
  (get navigational-status (bin-to-dec x)))

(defn bin-to-rate-of-turn
  [x]
  (rate-of-turn (twos x)))






(defn bin-to-dec
  [x]
  (Integer/parseInt x 2))

(bin-to-dec "101")

(defn bin-to-latlong
  "transforma de binario (complemento de 2) para latitude ou longitude"
  [x]
  (float (/ (twos x) 600000)))

;; teste
(bin-to-latlong (nth substrings01 8))
(bin-to-latlong (str "0" (Integer/toString (* 43 600000) 2)))

(defn bin-to-latitude
  [x]
  (let [latitude (bin-to-latlong x)]
    (cond (= (int latitude) 91) {:info "not available"}
          (<= 0 latitude 90) {:value latitude})))

(defn bin-to-longitude
  [x]
  (let [longitude (bin-to-latlong x)]
    (cond (= (int longitude) 181) {:info "not available"}
          (<= 0 longitude 180) {:value longitude})))

;;teste
(bin-to-longitude (str "0" 
                      (Integer/toString (* 181 600000) 2)))


(defn rate-of-turn
  [x]
   (let [value (Math/pow (/ x 4.733) 2)] 
     {:value value
      :direction (cond (< 0 x 127) :right
                       (< -127 x 0) :left
                       (= 0 x) 0)
      }))

(rate-of-turn (twos (nth substrings01 4)))

(rate-of-turn 126) 

(twos "1010101")

(defn bin-to-sog
  [x]
  (let [dec (Integer/parseInt x 2)
        sog (/ dec 10)]
    (cond (= dec 1023) {:info "not available"}
          (= dec 1022) {:info "102.2 knots or higher"}
          (< 0 dec 1022) {:value (float sog)})))

(pprint (map #(bin-to-timestamp (Integer/toBinaryString %)) '(50 59 60 61 62)))

(defn bin-to-position-accuracy
  [x]
  (cond (= x 1) :high
        (= x 0) :low))

(defn bin-to-cog
  [x]
  (let [dec (Integer/parseInt x 2)] 
    (cond (= dec 3600) {:info "not available"}
          (<= 0 dec 3599) {:value (float (/ dec 10))})))

(defn bin-to-true-heading
  [x]
  (let [dec (Integer/parseInt x 2)]
    (cond (= dec 511) {:info "not available"}
          (<= 0 dec 359) {:value dec})))

(defn bin-to-timestamp
  [x]
  (let [dec (Integer/parseInt x 2)]
    (cond (= dec 60) {:info "time stamp not available"}
          (= dec 61) {:info "positioning system is in manual input mode"}
          (= dec 62) {:info "electronic position fixing system operates in estimated (dead reckoning) mode"}
          (<= 0 dec 59) {:value dec})))

(defn bin-to-special-manoevre
  [x]
  (let [dec (Integer/parseInt x 2)]
    (cond (= dec 0) {:info "not available"}
          (= dec 1) {:info "not engaged in special manoeuvre"}
          (= dec 2) {:info "engaged in special manoeuvre"})))

(defn bin-to-raim-flag
  [x]
  (cond (= x 0) {:info "RAIM not in use"}
        (= x 1) {:info "RAIM in use"}))



(def navigational-status
  ["under way using engine",
   "at anchor",
   "not under command",
   "restricted maneuverability",
   "constrained by her draught",
   "moored",
   "aground",
   "engaged in fishing",
   "under way sailing",
   "reserved for future amendment of navigational status for ships carrying DG, HS, or MP, or IMO hazard or pollutant category C, high speed craft (HSC)",
   "reserved for future amendment of navigational status for ships carrying dangerous goods (DG), harmful substances (HS) or marine pollutants (MP), or IMO hazard or pollutant category A, wing in ground (WIG);11 = power- driven vessel towing astern (regional use)",
   "power-driven vessel pushing ahead or towing alongside (regional use)",
   "reserved for future use",
   "AIS-SART (active), MOB-AIS, EPIRB-AIS",
   "undefined"])



;; (s/def ::messageID-spec
;;   (s/and
;;    ::binario
;;    #(<= (Integer/parseInt % 2) 27)))

;; (s/def ::repeat-indicator-spec
;;   (s/and
;;    ::binario
;;    #(<= 0 (Integer/parseInt % 2) 3)))

;; (s/def ::rate-of-turn
;;   (s/and
;;    ::binario
;;    ))


;; (s/def ::message
;;   (s/cat :messageID ::messageID-spec
;;          :repeat-indicator ::repeat-indicator-spec
;;          :userID ::binario
;;          :navigational-status ::binario
         
;;          ))


;; two's complement

;; (defn inverte [x]
;;   (cond (= x \0) \1
;;        (= x \1) \0))

;; (defn map-inverte01 [x] 
;;   (map #(cond (= % \0) \1
;;               (= % \1) \0)
;;   x))

;; (defn twos-comp-to-dec [x]
;;   (let [sequencia (seq x)
;;         f (first sequencia)
;;         the-rest (rest sequencia)]
;;     (cond (= f \0) (Integer/parseInt (apply str the-rest) 2)
;;           (= f \1) (* -1 (+ 1 (Integer/parseInt 
;;                           (apply str (map-inverte01 the-rest))
;;                           2))))))

;; (twos-comp-to-dec "10000000")




(defn twos [x]
  (let [all-ones (Integer/parseInt 
                  (apply str 
                         (map (fn [x] \1) (seq x))) 2)
        length (.length x)
        dec (Integer/parseInt x 2)]
    (if (bit-test dec (- length 1)) 
          (* -1 (+ 1 (bit-xor dec all-ones)))
          dec)))

(twos "110000")



