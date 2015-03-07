(ns lc_3.assembler
  (:require [clojure.string :as str]))

(comment :overall
  "Observations about lc-3 assembly:"
  "1) each opcode accept a fixed number of arguments;"
  "2) tokens include: directive, opcode, register, immediate, label, comment;"
  "3) tokens are exclusively defined,"
  "   meaning that no lexeme belongs to more than one token,"
  "   except for hex number notation, such as x1;"
  "4) zero or one instruction per line;")

(def token-complex
  #{:id :decimal :string :comment})

(def token-directive
  #{:orig :fill :blkw :stringz :end})

(def token-opcode
  #{:add :and :br :brn :brnz :brnp :brnzp :brz :brzp :brp
    :jmp :jsr :jsrr :ld :ldi :ldr :lea :not :ret :rti :st :sti
    :str :trap :getc :out :puts :in :putsp :halt})

(def token-register
  #{:r0 :r1 :r2 :r3 :r4 :r5 :r6 :r7})

(def token-set
  (clojure.set/union token-complex
                     token-directive
                     token-opcode
                     token-register))

;;note: since peek on both init-q, which is empty, and (conj init-q nil),
;;which is non-empty, results nil, empty? is used to test the emptyness
;;of a queue.
(def init-q clojure.lang.PersistentQueue/EMPTY)

;;print-method implementation is from Joy of Clojure(2nd edition)
(defmethod print-method clojure.lang.PersistentQueue [queue writer]
  (print-method '<- writer)
  (print-method (seq queue) writer)
  (print-method '-< writer))

(def first-pass-data
  {:char-seq ()       ;input sequence of characters
   :token-seq init-q  ;FIFO queue that holds recognized tokens so far
   :errmsg-seq init-q ;FIFO queue that records encountered errors
   })

(defn line-scanner
  "turn a line of string into a queue of tokens"
  [s]
  (let [not-separator? ;doesn't consume the character
        (fn [c]
          (assert (char? c))
          (not (or (= c \,) (= c \;) (Character/isWhitespace c))))

        update-data
        (fn [curr s t e]
          (let [curr (if s (assoc curr :char-seq s) curr)
                curr (if t (assoc curr :token-seq (conj (:token-seq curr) t))
                         curr)
                curr (if e (assoc curr :errmsg-seq (conj (:errmsg-seq curr) e))
                         curr)]
            curr))
        
        skip-spaces
        (fn [curr]
          (let [s (:char-seq curr)
                c (first s)]
            (assert (or (Character/isWhitespace c) (= c \,)))
            (loop [s (rest s)]
              (let [c (first s)]
                (if (and c (or (Character/isWhitespace c) (= c \,)))
                  (recur (rest s))
                  (update-data curr s nil nil))))))

        read-comment ;consume the rest of the line as comment
        (fn [curr]
          (let [s (:char-seq curr)]
            (assert (= (first s) \;))
            (loop [s (rest s)
                   t []]
              (if (empty? s)
                (update-data curr s {:token :comment :value (str/join t)} nil)
                (recur (rest s) (conj t (first s)))))))

        ;;hex format tokens don't come here, since they start with \x
        ;;(a letter as any id tokens) in lc-3 instead of "0x" as in other languages
        read-decimal
        (fn [curr]
          (let [s (:char-seq curr)
                c (first s)]
            (assert (or (= c \#) (Character/isDigit c)))
            (loop [s (rest s)
                   t (if (= c \#) [] [c]) ;drop leading \# if there is one
                   all-digits? true]    ;remember encounterance of error(s),
                                        ;since loop doesn't stop at 1st error
              (let [c (first s)]
                (if (and c (not-separator? c)) ;read until separator or nil
                  (recur (rest s) (conj t c)
                         (and all-digits? (Character/isDigit c)))
                  (let [t (str/join t)]
                    (if all-digits?
                      (update-data curr s {:token :decimal :value t} nil)
                      (update-data curr s nil
                                   (str t " is not a well formatted number")))))))))

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
                (if (and c (not-separator? c))
                  (recur (rest s) (conj t c))
                  (let [x (str/join t)
                        sym (keyword (str/lower-case x))]
                    (if (or (token-register sym) (token-opcode sym))
                      (update-data curr s {:token sym} nil)
                      (update-data curr s {:token :id :value x} nil))))))))

        read-directive
        (fn [curr]
          (let [s (:char-seq curr)]
            (assert (= (first s) \.))
            (loop [s (rest s)
                   t []]
              (let [c (first s)]
                (if (and c (not-separator? c))
                  (recur (rest s) (conj t c))
                  (let [x (str/join t)
                        y (token-directive (keyword (str/lower-case x)))]
                    (case y
                      :end (if (empty? (:token-seq curr))
                             (update-data curr () {:token y} nil)
                             (update-data curr () nil
                                          (str "unexpected tokens preceding " y)))
                      nil  (update-data curr s nil
                                       (str "." x " is not a directive"))
                      (update-data curr s {:token y} nil))))))))

        read-string
        (fn [curr]
          (let [s (:char-seq curr)]
            (assert (= (first s) \"))
            (let [suc (second s)]
              (case suc
                nil (update-data curr () nil "string misses closing double-quote")
                \"  (update-data curr (rest s) {:token :string :value ""} nil)
                (loop [s (rest s)
                       t [suc]
                       consecutive-backslash-count 0]
                  (assert (not (empty? s)))
                  (if (empty? (rest s))
                    (update-data curr () nil
                                 (str "string " (str/join t)
                                      " misses closing double-quote"))
                    (let [c   (first s)
                          suc (second s)
                          cbc (if (= c \\) (inc consecutive-backslash-count)
                                  0)]
                      (if (and (= suc \") (even? cbc))
                        (update-data curr (rest (rest s))
                                     {:token :string :value (str/join t)} nil)
                        (recur (rest s) (conj t suc) cbc)))))))))]
    (loop [curr (assoc first-pass-data :char-seq s)]
      (let [{s :char-seq q :token-seq e :errmsg-seq} curr]
        (if (empty? s)
          curr ;including error messages
          (let [c (first s)]
            (cond
              (or (Character/isWhitespace c) (= c \,))
              (recur (skip-spaces curr)) ;\, is recognized as a space character
              
              (Character/isLetter c)
              (recur (read-id curr))
              
              (Character/isDigit c)
              (recur (read-decimal curr))
              
              :else
              (case c
                \; (recur (read-comment curr))
                \# (recur (read-decimal curr))
                \. (recur (read-directive curr))
                \" (recur (read-string curr))
                (recur
                 (update-data curr (rest s) nil
                              (str "invalid character " c " is found")))))))))))

(defn line-scan-reducer [v line]
  (let [ln   (inc (count v))
        curr (line-scanner line)]
    (do
      ;;print error messages if there is any
      (loop [e (:errmsg-seq curr)]
        (let [msg (peek e)]
          (if msg
            (do (println "Error at line" ln ":" msg ".")
                (recur (pop e))))))
      (let [q (:token-seq curr)]
        (if (= (:token (peek q)) :end) (reduced v)
            (conj v q))))))


(use 'clojure.java.io)
(defn first-pass
  "produce a vector of token queues, one queue for each line before .END"
  [path-to-file]
  (with-open [r (reader path-to-file)]
    (reduce line-scan-reducer [] (line-seq r))))

(def second-pass-data {:line-num nil
                       :label nil
                       :first nil  ;opcode or directive
                       :rest []    ;vector of arguments
                       :errmsg nil ;holds one or none error message
                       })

;;doesn't handle empty queue here
(defn token-queue-scanner [q]
  (comment
    "Identify first encountered :id(as label), opcode or directive in the given"
    "token queue, skip commas and the comment, and turn the token queue into"
    "second-pass-data.")
  (assert (and (= (type q) clojure.lang.PersistentQueue)
               (not (empty? q))))
  (loop [p :get-label
         q q
         r second-pass-data]
    (let [x (peek q) t (:token x)]
      (if (or (empty? q) (= t :comment)) ;loop ends at empty q or :comment
        r
        (case p
          :get-label ;try :get-first next if proceed
          (if (= t :id)
            (recur :get-first (pop q) (assoc r :label (:value x)))
            (recur :get-first q r))

          :get-first ;try :get-rest next if proceed
          (if (or (token-directive t) (token-opcode t))
            (recur :get-rest (pop q) (assoc r :first t))
            (assoc r :errmsg
                   "doesn't find directive or opcode at expected position"))

          :get-rest
          (if (or (#{:id :decimal :string} t) (token-register t))
            (recur :get-rest (pop q) (assoc r :rest (conj (:rest r) x)))
            (assoc r :errmsg (str "unexpected " t " token is found"))))))))

(defn token-queue-scan-reducer [v q]
  (let [ln   (inc (count v))
        curr (if (empty? q) nil (token-queue-scanner q))]
    (let [msg (:errmsg curr)]
      (if msg
        (do (println "Error at line" ln ":" msg ".")
            (conj v nil)) ;line with error results in a nil item in result vector
        (conj v (if curr (assoc curr :line-num ln)))))))

(defn second-pass
  "produce a vector of instruction maps, one for each token queue from first pass"
  [v]
  (reduce token-queue-scan-reducer [] v))

(defn rangeChecker [low high]
  (fn [x] (if (and (>= x low) (<= x high))
            x nil)))

;;range of a word
(def wordSpace?
  (let [limit (bit-shift-left 1 15)]
    (rangeChecker (- limit) (dec limit))))

;;range of memory address
(def memSpace? (rangeChecker 0 (dec (bit-shift-left 1 16))))

;;range of an imm5 value
(def imm5Space?
  (let [limit (bit-shift-left 1 4)]
    (rangeChecker (- limit) (dec limit))))

;;range of an offset6 value
(def offset6Space?
  (let [limit (bit-shift-left 1 5)]
    (rangeChecker (- limit) (dec limit))))

;;range of an offset9 value
(def offset9Space?
  (let [limit (bit-shift-left 1 8)]
    (rangeChecker (- limit) (dec limit))))

;;range of an offset11 value
(def offset11Space?
  (let [limit (bit-shift-left 1 10)]
    (rangeChecker (- limit) (dec limit))))

;;range of a trapvect8 value
(def trapSpace? (rangeChecker 0 (dec (bit-shift-left 1 8))))

(defn immediateTokenHelper [firstcall? s radix range]
  (try (let [x (Integer/parseInt s radix)]
         (range x))
       (catch NumberFormatException e
         (if firstcall?
           (let [h (.charAt s 0)
                 t (.substring s 1)]
             (if (or (= h \X) (= h \x))
               (immediateTokenHelper false t 16 range)))))))

(defn immediateToken [s] (immediateTokenHelper true s 10 wordSpace?))
(defn addressToken   [s] (immediateTokenHelper true s 10 memSpace?))
(defn imm5Token      [s] (immediateTokenHelper true s 10 imm5Space?))
(defn offset6Token   [s] (immediateTokenHelper true s 10 offset6Space?))
(defn offset9Token   [s] (immediateTokenHelper true s 10 offset9Space?))
(defn offset11Token  [s] (immediateTokenHelper true s 10 offset11Space?))
(defn trapvect8Token [s] (immediateTokenHelper true s 10 trapSpace?))

(def init-env {:orig nil :pc nil :label-coll {} :vec []})

(defn third-pass-reducer [env inst]
  (if (nil? inst)
    env
    (let [ln  (:line-num inst)
          pc  (:pc env)
          orig (:orig env)
          v (:vec env)
          
          add-label
          (fn [env s]
            (let [lmap (:label-coll env)
                  s    (str/lower-case s)]
              (assert (map? lmap))
              (if (lmap s)
                (do (println "Error at line" ln ":"
                             "label" s "is not uniquely defined.")
                    env)
                (->> (assoc lmap s pc) (assoc env :label-coll)))))

          set-orig
          (fn [env s]
            (let [addr (addressToken s)]
              (if addr
                (-> env (assoc :orig addr)
                    (assoc :pc addr) (assoc :vec [])) ;clear :vec
                (do (println "Error at line" ln ":"
                             s "is not a valid address token.")
                    init-env))))

          make-blkw
          (fn [env s  t]
            (assert (:pc env))
            (let [nwords (addressToken s)]
              (cond
                (nil? nwords)
                (do (println "Error at line" ln ":"
                             s "is not a valid block size token.")
                    env)

                (not (memSpace? (+ pc (dec nwords))))
                (do (println "Error at line" ln ":"
                             "block surpassses address space.")
                    env)

                (nil? t)
                (let [blkw (vec (repeat nwords 0))]
                  (-> (assoc env :vec (vec (concat v blkw)))
                      (assoc :pc (+ pc nwords))))

                :else
                (let [init (immediateToken t)]
                  (if (nil? init)
                    (do (println "Error at line" ln ":"
                                 t "is not a valid word value.")
                        env)
                    (let [blkw (vec (repeat nwords init))]
                      (-> (assoc env :vec (vec (concat v blkw)))
                          (assoc :pc (+ pc nwords)))))))))

          make-stringz
          (fn [env s]
            (assert (string? s))
            (let [nwords (inc (count s))] ;inc for appended termination word
              (if (not (memSpace? (+ pc (dec nwords))))
                (do (println "Error at line" ln ":"
                             "string" s "surpassses address space.")
                    env)
                (let [v-end (loop [s s
                                   v []]
                              (if (empty? s)
                                (conj v 0)
                                (recur (rest s) (conj v (first s)))))]
                  (-> (assoc env :vec (vec (concat v v-end)))
                      (assoc :pc (+ pc nwords)))))))

          make-fill
          (fn [env s]
            (let [init (immediateToken s)]
              (if (nil? init)
                (do (println "Error at line" ln ":"
                             s "is not a valid word value.")
                    env)
                (-> (assoc env :vec (conj v init))
                    (assoc :pc (inc pc))))))]
      (assert (or (and (nil? orig) (nil? pc))
                  (and (memSpace? orig) (memSpace? pc) (<= orig pc))))
      (if (nil? orig)
        (if (not (= (:first inst) :orig))
          (do (println "Error at line" ln ":"
                       ".ORIG is expected to be defined at first.")
              init-env)
          (let [s (:value ((:rest inst) 0))]
            (if (nil? s)
              (do (println "Error at line" ln ":"
                           "cannot find argument token for .ORIG.")
                  init-env)
              (set-orig env s))))
        (let [label    (:label inst)
              op       (:first inst)
              [x & xs] (:rest inst)
              env      (if label (add-label env label) env)]
          (case op
            :orig
            (do (println "Error at line" ln ":" "re-definition of .ORIG.")
                env)
            
            :blkw
            (let [s (:value x)
                  t (:value (first xs))]
              (if (nil? s)
                (do (println "Error at line" ln ":"
                             "cannot find block size token for .BLKW.")
                    env)
                (make-blkw env s t))) ;t can be nil
            
            :stringz
            (if (not (= (:token x) :string))
              (do (println "Error at line" ln ":" :stringz "expects"
                           "a string argument instead of" (:token x) ".")
                  env)
              (make-stringz env (:value x)))

            :fill
            (let [s (:value x)]
              (if (nil? s)
                (do (println "Error at line" ln ":"
                             "cannot find initial value for .FILL.")
                    env)
                (make-fill s)))

            (let [inst (-> inst (dissoc :errmsg) (dissoc :label))]
              (-> (assoc env :vec (conj v inst))
                  (assoc :pc (inc pc))))))))))

(defn third-pass [inst-v]
  (comment "compress the vector from second pass (drop nil items),"
           "produce the vector that resembles final memory layout,"
           "collect labels with their addresses.")
  (reduce third-pass-reducer init-env inst-v))

(defn label-to-pcoffset [env inst]
  (assert inst)
  (let [lset (:label-coll env)
        pc   (:pc env)
        ln   (:line-num inst)
        op   (:first inst)
        argv (:rest inst)]
    (condp get op
      #{:br :brn :brnz :brnp :brnzp :brz :brzp :brp :jsr}
      (let [k (:value (first argv))
            v (get lset (if (string? k)
                          (str/lower-case k)
                          nil))]
        (if (nil? v)
          (do (println "Error at line" ln ":"
                       op "expects valid label instead of" k ".")
              (-> env
                  (assoc :vec (conj (:vec env) nil))
                  (assoc :pc (inc pc))))
          (let [argv (vec (-> (rest argv)
                              (conj  {:token :decimal
                                      :value (str (- v pc 1))})))
                inst (assoc inst :rest argv)]
            (-> env
                (assoc :vec (conj (:vec env) inst))
                (assoc :pc (inc pc))))))
      
      #{:ld :ldi :lea :st :sti}
      (let [k (:value (second argv))
            v (get lset (if (string? k)
                          (str/lower-case k)
                          nil))]
        (if (nil? v)
          (do (println "Error at line" ln ":" op "expects"
                       "valid label as 2nd argument instead of" k ".")
              (-> env
                  (assoc :vec (conj (:vec env) nil))
                  (assoc :pc (inc pc))))
          (let [argv (vec (-> (rest (rest argv))
                              (conj {:token :decimal
                                     :value (str (- v pc 1))})
                              (conj (first argv))))
                inst (assoc inst :rest argv)]
            (-> env
                (assoc :vec (conj (:vec env) inst))
                (assoc :pc (inc pc))))))

      ;;either inst has :op other than the instructions that use labels,
      ;;or it is a word defined by .FILL, .BLKW, or .STRINGZ
      (-> env
          (assoc :vec (conj (:vec env) inst))
          (assoc :pc (inc pc))))))

(defn fourth-pass
  "replace labels used with corresponding pcoffset values"
  [env]
  (assert (and (:orig env) (:vec env)))
  (let [vec (:vec env)
        env (-> env
                (assoc :vec [])
                (assoc :pc (:orig env)))]
    (-> (reduce label-to-pcoffset env vec)
        (dissoc :pc) (dissoc :label-coll))))

;;data structure returned by register, offset6, offset9, etc. checkers.
(def checker-ret {:value nil :errmsg nil})

(defn gen-encoder [op argc & clauses]
  (assert (= (* argc 2) (count clauses)))
  (fn [inst]
    (assert (= (:first inst) op))
    (let [ln (:line-num inst)
          argv (:rest inst) c (count argv)]
      (if (not (= argc c))
        (println "Error at line" ln ":"
                 op "expects" argc "arguments instead of" c ".")
        (loop [i 1
               cs clauses
               xs argv
               r {:op op}
               eflag false]
          (assert (<= i (inc argc)))
          (if (empty? xs)
            (if eflag nil r)
            (let [x (first xs)
                  f (second cs)
                  field (first cs)
                  result (f x)
                  msg (:errmsg result)]
              (if msg
                (do (println "Error at line" ln ":" op "expects"
                             "argument" i "to be" msg ".")
                    (recur (inc i) (rest (rest cs)) (rest xs)
                           (assoc r field nil)
                           (or eflag true)))
                (recur (inc i) (rest (rest cs)) (rest xs)
                       (assoc r field (:value result))
                       (or eflag false))))))))))

(defn register [x]
  (let [t (:token x) v (token-register t)]
    (if v
      (assoc checker-ret :value
             (case v :r0 0 :r1 1 :r2 2 :r3 3 :r4 4 :r5 5 :r6 6 :r7 7))
      (assoc checker-ret :errmsg (str "register token instead of " t)))))

(defn imm5 [x]
  (let [s (:value x) v (if (string? s) (imm5Token s) nil)]
    (if v (assoc checker-ret :value v)
        (assoc checker-ret :errmsg (str "imm5 token instead of " s)))))

(defn offset6 [x]
  (let [s (:value x) v (if (string? s) (offset6Token s) nil)]
    (if v (assoc checker-ret :value v)
        (assoc checker-ret :errmsg (str "offset6 token instead of " s)))))

(defn offset9 [x]
  (let [s (:value x) v (if (string? s) (offset9Token s) nil)]
    (if v (assoc checker-ret :value v)
        (assoc checker-ret :errmsg (str "offset9 token instead of " s)))))

(defn offset11 [x]
  (let [s (:value x) v (if (string? s) (offset11Token s) nil)]
    (if v (assoc checker-ret :value v)
        (assoc checker-ret :errmsg (str "offset11 token instead of " s)))))

(defn trapvect8 [x]
  (let [s (:value x) v (if (string? s) (trapvect8Token s) nil)]
    (if v (assoc checker-ret :value v)
        (assoc checker-ret :errmsg (str "trapvect8 token instead of " s)))))

(defn encode-add [inst]
  (assert (= (:first inst) :add))
  (let [ln (:line-num inst)
        argv (:rest inst) c (count argv)]
    (if (not (= c 3))
      (println "Error at line" ln ":"
               :add "expects 3 arguments instead of" c ".")
      (loop [i 0
             xs argv
             r {}
             eflag false]
        (let [x (first xs) t (:token x)
              xs (rest xs)]
          (case i
            0 (let [result (register x) msg (:errmsg result)]
                (if msg (do (println "Error at line" ln ":" :add
                                     "expects argument 1 to be" msg ".")
                            (recur 1 xs r (or eflag true)))
                    (recur 1 xs (assoc r :dr result) (or eflag false))))

            1 (let [result (register x) msg (:errmsg result)]
                (if msg (do (println "Error at line" ln ":" :add
                                     "expects argument 2 to be" msg ".")
                            (recur 2 xs r (or eflag true)))
                    (recur 2 xs (assoc r :sr1 result) (or eflag false))))

            2 (let [result (register x)]
                (if (nil? (:errmsg result))
                  (recur 3 xs (-> (assoc r :sr2 (:value result))
                                  (assoc :op :add))
                         (or eflag false))
                  (let [result (imm5 x)]
                    (if (nil? (:errmsg result))
                      (recur 3 xs (-> (assoc r :imm5 (:value result))
                                      (assoc :op :add-i))
                             (or eflag false))
                      (do (println "Error at line" ln ":" :add "expects"
                                   "argument 3 be register or imm5 token"
                                   "instead of" t ".")
                          (recur 3 xs r (or eflag true)))))))

            3 (if eflag nil r)))))))

(defn encode-and [inst]
  (assert (= (:first inst) :and))
  (let [ln (:line-num inst)
        argv (:rest inst) c (count argv)]
    (if (not (= c 3))
      (println "Error at line" ln ":"
               :and "expects 3 arguments instead of" c ".")
      (loop [i 0
             xs argv
             r {}
             eflag false]
        (let [x (first xs) t (:token x)
              xs (rest xs)]
          (case i
            0 (let [result (register x) msg (:errmsg result)]
                (if msg (do (println "Error at line" ln ":" :and
                                     "expects argument 1 to be" msg ".")
                            (recur 1 xs r (or eflag true)))
                    (recur 1 xs (assoc r :dr result) (or eflag false))))

            1 (let [result (register x) msg (:errmsg result)]
                (if msg (do (println "Error at line" ln ":" :and
                                     "expects argument 2 to be" msg ".")
                            (recur 2 xs r (or eflag true)))
                    (recur 2 xs (assoc r :sr1 result) (or eflag false))))

            2 (let [result (register x)]
                (if (nil? (:errmsg result))
                  (recur 3 xs (-> (assoc r :sr2 (:value result))
                                  (assoc :op :and))
                         (or eflag false))
                  (let [result (imm5 x)]
                    (if (nil? (:errmsg result))
                      (recur 3 xs (-> (assoc r :imm5 (:value result))
                                      (assoc :op :and-i))
                             (or eflag false))
                      (do (println "Error at line" ln ":" :and "expects"
                                   "argument 3 be register or imm5 token"
                                   "instead of" t ".")
                          (recur 3 xs r (or eflag true)))))))

            3 (if eflag nil r)))))))

(def encode-br (gen-encoder :br 1 :pcoffset9 offset9))
(def encode-brn (gen-encoder :brn 1 :pcoffset9 offset9))
(def encode-brz (gen-encoder :brz 1 :pcoffset9 offset9))
(def encode-brp (gen-encoder :brp 1 :pcoffset9 offset9))
(def encode-brzp (gen-encoder :brzp 1 :pcoffset9 offset9))
(def encode-brnp (gen-encoder :brnp 1 :pcoffset9 offset9))
(def encode-brnz (gen-encoder :brnz 1 :pcoffset9 offset9))
(def encode-brnzp (gen-encoder :brnzp 1 :pcoffset9 offset9))
(def encode-jmp (gen-encoder :jmp 1 :baser register))
(def encode-jsr (gen-encoder :jsr 1 :pcoffset9 offset9))
(def encode-jsrr (gen-encoder :jsrr 1 :baser register))
(def encode-ld (gen-encoder :ld 2 :dr register :pcoffset9 offset9))
(def encode-ldi (gen-encoder :ldi 2 :dr register :pcoffset9 offset9))
(def encode-ldr (gen-encoder :ldr 3 :dr register :baser register :pcoffset6 offset6))
(def encode-lea (gen-encoder :lea 2 :dr register :pcoffset9 offset9))
(def encode-not (gen-encoder :not 2 :dr register :sr register))
(def encode-ret (gen-encoder :ret 0))
(def encode-rti (gen-encoder :rti 0))
(def encode-st (gen-encoder :st 2 :sr register :pcoffset9 offset9))
(def encode-sti (gen-encoder :sti 2 :sr register :pcoffset9 offset9))
(def encode-str (gen-encoder :str 3 :sr register :baser register :offset6 offset6))
(def encode-trap (gen-encoder :trap 1 :trapvect8 trapvect8))

(defn trap-unalias [op x]
  (fn [inst]
    (let [f (gen-encoder op 0)
          r (f inst)]
      (if r {:op :trap :trapvect8 x}))))
(def encode-getc (trap-unalias :getc 32))
(def encode-out (trap-unalias :out 33))
(def encode-puts (trap-unalias :puts 34))
(def encode-in (trap-unalias :in 35))
(def encode-putsp (trap-unalias :putsp 36))
(def encode-halt (trap-unalias :halt 37))

(defn encode [v inst]
  (assert (vector? v))
  (let [op (:first inst) ;op can be nil
        f (case op
            :add encode-add
            :and encode-and
            :br encode-br
            :brn encode-brn
            :brnz encode-brnz
            :brnp encode-brnp
            :brnzp encode-brnzp
            :brz encode-brz
            :brzp encode-brzp
            :brp encode-brp
            :jmp encode-jmp
            :jsr encode-jsr
            :jsrr encode-jsrr
            :ld encode-ld
            :ldi encode-ldi
            :ldr encode-ldr
            :lea encode-lea
            :not encode-not
            :ret encode-ret
            :rti encode-rti
            :st encode-st
            :sti encode-sti
            :str encode-str
            :trap encode-trap
            :getc encode-getc
            :out encode-out
            :puts encode-puts
            :in encode-in
            :putsp encode-putsp
            :halt encode-halt
            nil (fn [x] x))]
    (conj v (f inst))))

(defn fifth-pass
  "encode each item in the vec"
  [env]
  (let [orig (:orig env)
        v (:vec env)]
    (assert (loop [t true
                   v v]
              (if (empty? v) t
                  (recur (and t (first v)) (rest v)))))
    {:orig orig
     :mem (reduce encode [] v)}))

(defn assemble [path-to-file]
  (-> path-to-file
      first-pass
      second-pass
      third-pass
      fourth-pass
      fifth-pass))
