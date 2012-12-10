(ns urkle.core
  (:use [clojure.string :as str :only [join]])
  (:use [clojure.set])
  (:use [server.socket]))

(declare *users* *channels* *name* server-name)

(def *users* (ref {}))

(def *channels* (ref {}))

(def server-name
  (.getCanonicalHostName (java.net.InetAddress/getLocalHost)))

;Taken from http://clj-me.blogspot.com/2009/01/try-or-or-try-try-else-or-else-try.html
;as hinted on #clojure.
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

(defstruct channel :topic :users)



;Replies and Errors. See e.g. http://www.mirc-support.de/reference/raw.nameidx.htm
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
  ([reply trailing]
          (str/join " " 
                    (vals (struct-map message 
                                :prefix (str ":" server-name)
                                :command (format "%03d" (reply replies))
                                :trailing (str ":" trailing)))))
  ([reply params trailing]
          (str/join " " 
                    (vals (struct-map message 
                                :prefix (str ":" server-name)
                                :command (format "%03d" (reply replies))
                                :params params
                                :trailing (str ":" trailing))))))

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
    (str/join " "
              (vals (merge msg {:prefix (str ":" server-name) :command "PONG" :trailing (str ":" *name*)})))))

(defmethod relay "NICK" [msg]
  "Set/change nick name"
  (dosync (commute *users* conj {(keyword (:params msg)) *out*}))
  ; Return the name to bind it to *name*
  (:params msg))

(defmethod relay "JOIN" [msg]
  "Join a channel or create it"
  (dosync
    (let [chan (:params msg)]
    (if (contains? @*channels* (keyword chan))
      (commute *channels* update-in [(keyword chan) :users] conj (keyword *name*)) 
      (commute *channels* assoc-in [(keyword chan)] (struct-map channel :users #{(keyword *name*)}))))))

(defmethod relay "USER" [msg]
  (println
    (reply :rpl_welcome  *name* "This is Urkle IRC. The awkward ircd written in Clojure."))
  (println
    (reply :rpl_yourhost  *name* (str/join " " ["Your host is" server-name "running super alpha stuff."]))))

(defmethod relay "PRIVMSG" [msg]
  "PRIVMSG user|#channel."
  (let [recipient (:params msg)]
    (let [message (str/join " " (vals (merge msg {:prefix (str ":" *name*)})))]
      (if (.startsWith recipient "#")
        ;Let's assume we have a channel
        (doseq [user (clojure.set/difference (:users ((keyword recipient) @*channels*)) #{(keyword *name*)})]
          (binding [*out* (user @*users*)]
            (println message)))
        ;Just PRIVMSGing a user
        (binding [*out* ((keyword recipient) @*users*)]
          (println message))))))

(defmethod relay "LIST" [msg]
  "LIST all known channels."
  (println (reply :rpl_liststart  (str *name* " " "Channel") "Users Topic"))
  (when (not (empty? @*channels*))
    (doseq [chan @*channels*]
      (println (reply :rpl_list (str *name* " " (name (key chan)) " " (count (:users (val chan)))) ""))))
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
