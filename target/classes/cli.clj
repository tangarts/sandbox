(ns cli)

; https://vlaaad.github.io/tools-cli-in-10-lines-of-code

(defn echo [& args]
  (apply prn args))


(defn -main [& opts]
  (let [f #(try
             (let [form (read-string %)]
               (cond
                 (qualified-symbol? form) @(requiring-resolve form)
                 (symbol? form) @((ns-publics (symbol (namespace `-main))) form)
                 :else form))
             (catch Exception _ %))
        [f & args] (map f opts)]
    (some-> (apply f args) prn)))

