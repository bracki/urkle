(ns urkle.test
  (:use [urkle main])
  (:use [clojure.contrib test-is]))

(deftest test-re-trailing
  (is (= ":hi there." (re-trailing "PRIVMSG Berthold :hi there.")))
  (is (= ":hi there." (re-trailing "PRIVMSG #channel,#clojure asdasd :hi there.")))
  (is (nil? (re-trailing "PRIVMSG Berthold"))))

(run-tests)
