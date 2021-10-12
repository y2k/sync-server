(defn clear [t d]
  (if (= d "")
    t
    (clear (replace t (substr d 0 1) "")
           (substr d 1))))

(defn main [db config]
  (let [dirs (:directories config)]
    (doseq dirs (fn [dir] (uploadNewFiles (:key config) dir)))))

(defn uploadNewFiles [dir-last-updated key dir]
  (let [files (io/list-of-files dir)]
    (->
     files
     (filter (fn [f] (> (:last-updated f) dir-last-updated)))
     (doseq (fn [f] (uploadFile key f))))))

(defn uploadFile [key file]
  (let [data (aesEncodeFile file key)]
    (remote-call "add-new-file" file data)))
