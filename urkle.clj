(use '(clojure.contrib server-socket duck-streams str-utils seq-utils stacktrace))

(declare *users* *channels* *name* server-name)

(def *users* (ref {}))

(def *channels* (ref {}))

(def server-name "werner.coremedia.com")


(defmacro try-or
  "Evaluates exprs one at a time, from left to right. If a form returns a 
  value, this value is returned. If a form throws an exception, the next 
  form is evaluated. 
  If the last form throws an exception, the exception isn't caught." 
  ([] nil)
  ([form] form)
  ([form & forms]
         `(try 
            ~form
            (catch Exception e#
              (try-or ~@forms)))))

(defstruct message :prefix :command :params :trailing)

;Replies and Errors. See e.g. http://www.mirc-support.de/reference/raw.nameidx.htm"
(def replies 
  {:rpl_welcome 1
   :rpl_yourhost 2
   :rpl_created 3
   :rpl_myinfo 4
   :rpl_protocol 5
   :rpl_liststart 321
   :rpl_list 322
   :rpl_listend 323
   :err_unkowncommand 421
   })

(defn reply
  "Send a reply like:
  001 bracki :Your welcome!"
  ([reply msg]
          (str-join " " 
                    (vals (struct-map message 
                                :prefix (str ":" server-name)
                                :command (format "%03d" (reply replies))
                                :trailing (str ":" msg)))))
  ([reply recipient msg]
          (str-join " " 
                    (vals (struct-map message 
                                :prefix (str ":" server-name)
                                :command (format "%03d" (reply replies))
                                :params recipient
                                :trailing (str ":" msg))))))

(defn re-prefix [msg]
  "Find a <prefix>."
  (.trim (re-find #"^:[\w.]+\s" msg)))

(defn re-command [msg]
  "Find a <command>."
  (last (re-find #"(?:\s|^)(\w+)" msg)))

(defn re-params [msg]
  "Find the <params>."
  (last 
    (re-find #"\s([\S]+)(?:\s\:|$)" msg)))

(defn re-trailing [msg]
  "Find <trailing>."
  (last (re-find #"(?:\s)(\:.*)" msg)))

(defn parse-msg [msg]
  "Return a <prefix> <command> <params> <trailing> struct"
  (struct-map message
              :command (try-or (re-command msg) nil)
              :params (try-or (re-params msg) nil)
              :trailing (try-or (re-trailing msg) nil)))

(defn extract-args [cmd]
  (apply str (next (.split cmd " " 2))))

(defmulti relay :command)

(defmethod relay :default [msg]
  "Unkown Command."
  (println (reply :err_unkowncommand (:command msg) "Unknown Command.")))

(defmethod relay "PING" [msg]
  "Answer with PONG."
  (println
    (str-join " "
              (vals (merge msg {:prefix (str ":" server-name) :command "PONG" :trailing (str ":" *name*)})))))

(defmethod relay "NICK" [msg]
  "Set/change nick name"
  (dosync (commute *users* conj {(keyword (:params msg)) *out*}))
  ; Return the name to bind it to *name*
  (:params msg))

(defmethod relay "JOIN" [msg]
  "Join a channel or create it"
  (dosync
    (let [channel (:params msg)]
    (if (contains? @*channels* (keyword channel))
      (commute *channels* update-in [(keyword channel)] conj (keyword *name*)) 
      (commute *channels* conj {(keyword channel) [(keyword *name*)]})))))

(defmethod relay "USER" [msg]
  (println
    (reply :rpl_welcome  *name* "This is Urkle IRC. The awkward ircd written in Clojure."))
  (println
    (reply :rpl_yourhost  *name* "Your host is werner.coremedia.com running super alpha stuff.")))

(defmethod relay "PRIVMSG" [msg]
  (let [recipient (:params msg)]
    (let [message (str-join " " (vals (merge msg {:prefix (str ":" *name*)})))]
      (if (.startsWith recipient "#")
        (doseq [user ((keyword recipient) @*channels*)]
          (binding [*out* (user @*users*)]
            (println message)))
        (binding [*out* ((keyword recipient) @*users*)]
          (println message))))))

(defmethod relay "LIST" [msg]
  "LIST all known channels."
  (println (reply :rpl_liststart  (str *name* " " "Channel") "Users Topic"))
  (when (not (empty? @*channels*))
    (doseq [channel @*channels*]
      (println (reply :rpl_list (str *name* " " (name (key channel)) " " (count (val channel))) ""))))  
  (println (reply :rpl_listend *name* "End of /LIST")))

(defn- urkle-handle-client [in out]
  (binding [*in* (reader in)
            *out* (writer out)]
    (binding [*name* (relay (parse-msg (read-line)))]
      (loop [input (read-line)]
        (when input
          (relay (parse-msg input))
          (recur (read-line)))))))

(defonce server (create-server 3333 urkle-handle-client))
