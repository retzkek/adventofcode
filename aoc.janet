(use spork)

(defn input [year day]
  (with [f (file/open (path/join (os/getenv "HOME")
                 ".config"
                 "aocd"
                 (string year)
                 (string day)
                 "input.txt"))]
      (file/read f :all)))

(defn load-token []
  (with [f (file/open (path/join (os/getenv "HOME")
                 ".config"
                 "aocd"
                 "token"))]
      (file/read f :all)))

(defn fetch-input [year day]
  (let [url (string "https://adventofcode.com/"
                    (string year)
                    "/day/"
                    (string day)
                    "/input")
        r (http/request "GET"
                        url
                        :headers {"Cookie" (string "session=" (load-token))})]
    (r :body)))
