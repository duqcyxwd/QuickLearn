(ns chuan.async
  (:require [clojure.core.async :refer [go go-loop chan close! put! buffer timeout
                                        dropping-buffer alt! alts! >! <! <!! >!!]]))

;; TODO Update this to practice project

(require '[clojure.core.async :as a :refer [alts!]])

;; >!! <!!
(def c (chan))

#_(>!! c "Blocking - not in go-block")
#_(<!! c)


;; >! <!
;; go loop one time
(go-loop [data (<! c)]
  (println "Waited for => " data)
  (println "No recur. Won't print again"))

;; collect data 5 times
(go-loop [count 1]
  (println (str "retrieve loop " count ": " (<! c)))
  (when (< count 5)
    (recur (inc count))))

(put! c "Example Async Data")
(go (>! c "Example Async Data"))

(go
  (println "test")
  (let [res (<! c)]
    (print (str "Data " res))))


;; put! get!

;; Sample go code
(doseq [n (range 10)
        :let [i (-> n
                    inc
                    range
                    rand-nth)]]
  (go
    (<! (a/timeout (* n 1000)))
    (println n)))

;; alt!
(def trade-ch (chan))


;; alt! put
;; attempts to put trade on channel trade-ch within 1 second.
;; yields :timed-out or :sent.
(go
  (let [timeout-ch (timeout 1000)]
    (alt!
      timeout-ch :timed-out
      ;; note use of double-nested vector; [trade-ch trade]
      ;; would be interpreted as two channels to take from, resulting
      ;; in an odd error.
      [[trade-ch "trade"]] :sent)))

(go (alt! [[trade-ch "trade alt! simple send"]] :sent))
(go (>! trade-ch "trade by >!"))
(put! trade-ch "trade by put")

(go (let [res (<! trade-ch)]
      (print (str "Data " res))))
;; alt receive
(go (->> (alt! trade-ch ([result] (println (str "alt received result: " result))))
         println))
;; 2
(go-loop []
  (<! (timeout 1000))
  (println (str "retrieve loop" (<! trade-ch)))
  (recur))


;; alts!
(let [f (fn [x ch] (go (Thread/sleep (rand 100))
                       (>! ch x)))
      f2 (fn [x ch] (go (Thread/sleep 1000)
                        (>! ch x)))
      a (chan)
      b (chan)
      c (chan)
      chs [a b c]]
  (println "----------")
  (f 11 a)
  (f 1 a)
  (f 2 b)
  (f 3 c)
  (f2 1000 c)
  (println ":----------")
  ;; (Thread/sleep 200)              ; if this is commented out, it returns the
  (loop []
    (let [[n ch2] (a/alts!! chs)]
      (println "received: " n)
      (if (< n 100)
        (recur)))))

;; Notes go vs no go
;; go can used with single retrieve like <! alt! >!
;; <!! >!! doesn't need go, will block until data is retrieve


;; chan close
(close! chan)

(def c (chan))

(go (>! c "c by >!"))
(put! c "c by put")

(go (let [res (<! c)]
      (print (str "GetData " res))))

(def c-wait (go-loop [data (<! c)]
              (println "Waited for => " data)
              (println "No recur. Won't print again")))
(go (>! c "c by >!"))
(close! c-wait)
(close! c)


(def c (chan))
(def c-wait-l (go-loop [count 1]
                (println (str "retrieve loop " count ": " (<! c)))
                (when (< count 5)
                  (recur (inc count)))))

(put! c "Example Async Data")
(go (>! c "Example Async Data"))
(close! c-wait-l)
(close! c)
c-wait-l


(put! c "Example Async Data")
(go (>! c "Example Async Data"))

(go
  (println "test")
  (let [res (<! c)]
    (print (str "Data " res))))


(let [c (chan 2) ]
  (>!! c 1)
  (>!! c 2)
  (close! c)
  (println (<!! c)) ; 1
  (println (<!! c)) ; 2
  ;; since we closed the channel this will return false(we can no longer add values)
  (>!! c 1))
