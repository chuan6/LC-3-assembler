(ns lc_3.assembler
  (:require [clojure.string :as str]))

(comment :overall
  "Observations about lc-3 assembly:"
  "1) each opcode accept a fixed number of arguments;"
  "2) tokens include: directive, opcode, register, immediate, label, comment;"
  "3) tokens are exclusively defined,"
  "   meaning that no lexeme belongs to more than one token;"
  "4) zero or one instruction per line;")

(def token-complex
  #{'id 'decimal 'string 'comment})

(def token-directive
  #{'orig 'fill 'blkw 'stringz 'end})

(def token-opcode
  #{'add 'and 'br 'brn 'brnz 'brnp 'brnzp 'brz 'brzp 'brp
    'jmp 'jsr 'jsrr 'ld 'ldi 'ldr 'lea 'not 'ret 'rti 'st 'sti
    'str 'trap 'getc 'out 'puts 'in 'putsp 'halt})

(def token-register
  #{'r0 'r1 'r2 'r3 'r4 'r5 'r6 'r7})

(def token-set
  (clojure.set/union token-complex
                     token-directive
                     token-opcode
                     token-register))

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
          (let [s (:char-seq curr)
                c (first s)]
            (assert (or (Character/isWhitespace c) (= c \,)))
            (loop [s (rest s)]
              (let [c (first s)]
                (if (and c (or (Character/isWhitespace c) (= c \,)))
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
                                        {:token 'decimal :value t})}
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
                                          sym (symbol (str/lower-case x))]
                                      (if (or (token-register sym) (token-opcode sym))
                                        {:token sym}
                                        {:token 'id :value x})))})))))

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
                           (symbol (str/lower-case x)))
                        tseq (:token-seq curr)]
                    (case y
                      'end
                      (let [curr (assoc curr :char-seq ())]
                        (if (not (empty? tseq))
                          (assoc curr :errmsg-seq
                                 (conj (:errmsg-seq curr)
                                       (str "unexpected tokens preceding " y)))
                          (assoc curr :token-seq (conj tseq {:token y}))))
                      
                      nil (let [curr (assoc curr :char-seq s)]
                            (assoc curr :errmsg-seq
                                   (conj (:errmsg-seq curr)
                                         (str "." x " is not a directive"))))

                      {:char-seq s
                       :token-seq (conj (:token-seq curr) {:token y})})))))))

        read-string
        (fn [curr]
          (assert (= (first (:char-seq curr)) \"))
          (let [s (rest (:char-seq curr))
                token-seq (:token-seq curr)]
            (if (= (first s) \") ;if true, empty string found
              {:char-seq (rest s)
               :token-seq (conj token-seq {:token 'string :value ""})}
              (loop [s s
                     t []
                     consecutive-backslash-count 0]
                (if (or (empty? s) (empty? (rest s)))
                  (let [curr (assoc curr :char-seq ())]
                    (assoc curr :errmsg-seq
                           (conj (:errmsg-seq curr)
                                 (str "string " (str/join t)
                                      " misses closing double-quote"))))
                  (let [c    (first s)
                        peek (second s) ;c and peek are non-nil
                        cbc  (if (= c \\) (inc consecutive-backslash-count)
                                 0)]
                    (if (and (= peek \") (even? cbc))
                      {:char-seq (rest (rest s))
                       :token-seq (conj token-seq
                                        {:token 'string :value (str/join (conj t c))})}
                      (recur (rest s) (conj t c) cbc))))))))]
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
              (or (Character/isWhitespace c) (= c \,))
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
                \" (recur (read-string curr))
                (recur
                 (let [curr (assoc curr :char-seq (rest s))]
                   (assoc curr :errmsg-seq
                          (conj (:errmsg-seq curr)
                                (str "invalid character " c " is found")))))))))))))

(defn line-scan-reducer [v line]
  (let [line-num (inc (count v))
        curr (line-scanner line)]
    (do
      ;;print error message if there is any
      (loop [e (:errmsg-seq curr)]
          (let [msg (peek e)]
            (if msg
              (do (println "Error at line" line-num ":" msg ".")
                  (recur (pop e))))))
      (let [tseq (:token-seq curr)]
        (if (= (:token (peek tseq)) 'end)
          (reduced v)
          (conj v (:token-seq curr)))))))


(use 'clojure.java.io)
(defn first-pass
  "produce a vector of token queues, one queue for each line of the file"
  [path-to-file]
  (with-open [r (reader path-to-file)]
    (reduce line-scan-reducer [] (line-seq r))))

;;doesn't handle empty queue here
(defn token-queue-scanner [q]
  (comment
    "identify first encountered 'id(as label), opcode or directive in the given"
    "token queue, skip commas and the comment, and turn the token queue into"
    "a map, such as:"
    {:first 'add :label "LOOP"
     :rest [{:token 'id :value "R1"},
            {:token 'id :value "R1"},
            {:token 'id :value "x-1"}
            :errmsg nil]}
    "or" {:first 'orig :rest []})
  (assert (and (= (type q) clojure.lang.PersistentQueue)
               (not (empty? q))))
  (loop [p :get-label
         q q
         r {:first nil :label nil :rest [] :errmsg nil}]
    (let [t (peek q)]
      (if (or (nil? t) (= (:token t) 'comment))
        r
        (let [sym (:token t)]
          (case p
            :get-label ;try :get-first next if proceed
            (if (= sym 'id)
              (recur :get-first (pop q)
                     (assoc r :label (:value t)))
              (recur :get-first q r))

            :get-first ;try :get-rest next if proceed
            (if (or (token-directive sym) (token-opcode sym))
              (recur :get-rest (pop q)
                     (assoc r :first sym))
              (assoc r :errmsg
                     "doesn't find directive or opcode at expected position"))

            :get-rest
            (if (or (= sym 'id) (= sym 'decimal) (= sym 'string) (token-register sym))
              (recur :get-rest (pop q)
                     (assoc r :rest (conj (:rest r) t)))
              (assoc r :errmsg
                     (str "unexpected " sym " token is found")))))))))

(defn token-queue-scan-reducer [v q]
  (let [curr (if (empty? q) nil (token-queue-scanner q))
        line-num (inc (count v))]
    (let [msg (:errmsg curr)]
      (if msg
        (do (println "Error at line" line-num ":" msg ".")
            (conj v nil))
        (conj v (if curr (assoc curr :line-num line-num)))))))

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
(defn trapvect8Token [s] (immediateTokenHelper true s 10 trapSpace?))

(def init-env {:pc nil :label-coll {} :vec []})

(defn third-pass-reducer [env inst]
  (if (nil? inst)
    env
    (let [ln  (:line-num inst)
          pc  (:pc env)
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
                (-> env (assoc :pc addr) (assoc :vec [])) ;clear :vec
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
      (assert (or (nil? pc) (memSpace? pc)))
      (if (nil? pc)
        (if (not (= (:first inst) 'orig))
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
          (condp = op
            'orig
            (do (println "Error at line" ln ":" "re-definition of .ORIG.")
                env)
            
            'blkw
            (let [s (:value x)
                  t (:value (first xs))]
              (if (nil? s)
                (do (println "Error at line" ln ":"
                             "cannot find block size token for .BLKW.")
                    env)
                (make-blkw env s t))) ;t can be nil
            
            'stringz
            (if (not (= (:token x) 'string))
              (do (println "Error at line" ln ":" 'stringz "expects"
                           "a string argument instead of" (:token x) ".")
                  env)
              (make-stringz env (:value x)))

            'fill
            (let [s (:value x)]
              (if (nil? s)
                (do (println "Error at line" ln ":"
                             "cannot find initial value for .FILL.")
                    env)
                (make-fill s)))

            (-> (assoc env :vec (conj v inst))
                (assoc :pc (inc pc)))))))))

(defn third-pass [inst-v]
  (reduce third-pass-reducer init-env inst-v))

