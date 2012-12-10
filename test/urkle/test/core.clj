(ns urkle.test.core
  (:use [urkle.core])
  (:use [clojure.test]))

(deftest test-re-trailing
  (is (= ":hi there." (re-trailing "PRIVMSG Berthold :hi there.")))
  (is (= ":hi there." (re-trailing "PRIVMSG #channel,#clojure asdasd :hi there.")))
  (is (nil? (re-trailing "PRIVMSG Berthold"))))

