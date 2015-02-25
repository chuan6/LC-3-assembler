(ns lc_3.assembler
  (:require [clojure.string :as str]))

(comment :overall
  "Observations about lc-3 assembly:"
  "1) each opcode accept a fixed number of arguments;"
  "2) tokens include: directive, opcode, register, immediate, label, comment;"
  "3) tokens are exclusively defined,"
  "   meaning that no lexeme belongs to more than one token;"
  "4) zero or one instruction per line;")

(def line-vec [])

(def token-complex
  #{'id 'decimal 'string 'comment})

(def token-directive
  #{'orig 'fill 'blkw 'stringz 'end})

(def token-opcode
  #{'add 'and 'br 'brn 'brnz 'brnp 'brnzp 'brz 'brzp 'brp
    'jmp 'jsr 'jsrr 'ld 'ldi 'ldr 'lea 'not 'ret 'rti 'st 'sti
    'str 'trap 'getc 'out 'puts 'in 'putsp 'halt})

(def token-set
  (clojure.set/union #{'comma}
                     token-complex
                     token-directive
                     token-opcode))

;;print-method implementation is from Joy of Clojure(2nd edition)
(defmethod print-method clojure.lang.PersistentQueue [queue writer]
  (print-method '<- writer)
  (print-method (seq queue) writer)
  (print-method '-< writer))

(defn line-scanner
  "turn a line of string into a queue of tokens"
  [s]
  (let [separator?
        (fn [c]
          (assert (char? c))
          (or (= c \,) (= c \;) (Character/isWhitespace c)))
        
        skip-spaces
        (fn [curr]
          (let [s (:char-seq curr)]
            (assert (Character/isWhitespace (first s)))
            (loop [s (rest s)]
              (let [c (first s)]
                (if (and c (Character/isWhitespace c))
                  (recur (rest s))
                  (assoc curr :char-seq s))))))

        read-comment ;consume the rest of the line as comment
        (fn [curr]
          (let [s (:char-seq curr)]
            (assert (= (first s) \;))
            (loop [s (rest s)
                   t []]
              (if (empty? s)
                {:char-seq s
                 :token-seq (conj (:token-seq curr)
                                  {:token 'comment :value (str/join t)})}
                (recur (rest s) (conj t (first s)))))))

        ;;hex format token cannot be determined by the scanner, since
        ;;it starts with \x in lc-3 instead of "0x" as in other languages
        read-decimal
        (fn [curr]
          (let [s (:char-seq curr)
                c (first s)]
            (assert (or (= c \#) (Character/isDigit c)))
            (loop [s (rest s)
                   t (if (= c \#) [] [c])
                   valid true]
              (let [c (first s)]
                (if (and c (not (separator? c))) ;read until separator or nil
                  (recur (rest s) (conj t c)
                         (and (Character/isDigit c) valid))
                  (let [t (str/join t)]
                    (if valid
                      {:char-seq s
                       :token-seq (conj (:token-seq curr)
                                        {:token 'decimal :valur t})}
                      (let [curr (assoc curr :char-seq s)]
                        (assoc curr :errmsg-seq
                               (conj (:errmsg-seq curr)
                                     (str t " is not a well formatted number")))))))))))

        ;;read a label, an opcode, a register, or a hex format number
        ;;Note: syntax of label in lc-3 is not constructively defined.
        read-id
        (fn [curr]
          (let [s (:char-seq curr)
                c (first s)]
            (assert (Character/isLetter c))
            (loop [s (rest s)
                   t [c]]
              (let [c (first s)]
                (if (and c (not (separator? c)))
                  (recur (rest s) (conj t c))
                  {:char-seq s
                   :token-seq (conj (:token-seq curr)
                                    (let [x (str/join t)
                                          y (token-opcode
                                             (symbol (str/lower-case x)))]
                                      (if y
                                        {:token y}
                                        {:token 'id :name x})))})))))

        read-directive
        (fn [curr]
          (let [s (:char-seq curr)]
            (assert (= (first s) \.))
            (loop [s (rest s)
                   t []]
              (let [c (first s)]
                (if (and c (not (separator? c)))
                  (recur (rest s) (conj t c))
                  (let [x (str/join t)
                        y (token-directive
                           (symbol (str/lower-case x)))]
                    (if y
                      {:char-seq s
                       :token-seq (conj (:token-seq curr) {:token y})}
                      (let [curr (assoc curr :char-seq s)]
                        (assoc curr :errmsg-seq
                               (conj (:errmsg-seq curr)
                                     (str "." x " is not a directive")))))))))))]
    (loop [curr {:char-seq s
                 :token-seq clojure.lang.PersistentQueue/EMPTY
                 :errmsg-seq clojure.lang.PersistentQueue/EMPTY}]
      (let [s (:char-seq curr)
            q (:token-seq curr)
            e (:errmsg-seq curr)]
        (if (empty? s)
          curr ;including error messages
          (let [c (first s)]
            (cond
              (Character/isWhitespace c)
              (recur (skip-spaces curr))

              (Character/isLetter c)
              (recur (read-id curr))

              (Character/isDigit c)
              (recur (read-decimal curr))

              :else
              (case c
                \; (recur (read-comment curr))
                \# (recur (read-decimal curr))
                \. (recur (read-directive curr))
                \, (recur {:char-seq (rest s)
                           :token-seq (conj (:token-seq curr)
                                            {:token 'comma})})
                (recur (let [curr (assoc curr :char-seq (rest s))]
                         (assoc curr :errmsg-seq
                                (conj (:errmsg-seq curr)
                                      (str "invalid character " c " is found")))))))))))))

(defn line-scan-reducer [vec line]
  (let [curr (line-scanner line)
        line-num (inc (count vec))]
    (do
      ;;print error message if there is any
      (loop [e (:errmsg-seq curr)]
          (let [msg (peek e)]
            (if msg
              (do (println "Error at line" line-num ":" msg ".")
                  (recur (pop e))))))
      (conj vec (:token-seq curr)))))


(use 'clojure.java.io)
(defn scan
  "produce a vector of token queues, corresponding to each line of the file"
  [path-to-file]
  (with-open [r (reader path-to-file)]
    (reduce line-scan-reducer [] (line-seq r))))
