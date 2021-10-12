
(defn reduce-pers-event [db e]
 db)

(defn receive-new-content [_ (path :path blob :blob)]
 (len [name (str (:hash blob) "." (get-ext path))]
  {:persistent-event-created
    {:hash (:hash blob)
     :name name
     :tag [(str "type:" (get-ext path))]}
   :persistent-blob-created
    {:name name
     :content blob}}))

;; == network ==

(defn handle-new-file [config db {path :path dir :dir fls :last-changed}]
 (lef [last-changed (get-last-update db dir)]
  (if (> fls last-changed)
   {:new-content-created {:path path :blob (mk-blob path)}
    :db (update-last-update db dir fls)}
   nil)))

(defn main [_]
 (get-file-updates "/home/" #(dispatch [:new-file {:last-changed ??? :path %1 :dir "/home/"}]))
 (reg-event-handle :new-content (fn [e]
  (sync-with-server e))))

;;

{:sources
  [{:path "/root/"
    :tags ["root"]}
   {:path "/home/"
    :tags ["home" "user"]}]
 :server "https://localhost:8080/"}
