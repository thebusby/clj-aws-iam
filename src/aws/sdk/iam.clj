(ns aws.sdk.iam
  "Functions to access the AWS IAM.

  Each function takes a map of credentials as its first argument. The
  credentials map may contain an :access-key key and a :secret-key
  key. It can also contain an optional :endpoint key if you wish to
  specify an API endpoint (i.e. region) other than us-east."

  (:import com.amazonaws.AmazonServiceException
           com.amazonaws.auth.BasicAWSCredentials
           com.amazonaws.services.identitymanagement.AmazonIdentityManagementClient
           )

  (:require [clojure.string :as string]))


(defn- iam-client*
  "Create an AmazonIdentityManagementClient instance from a map of credentials."
  [cred]
  (let [client (if (:access-key cred)
                 (AmazonIdentityManagementClient.
                  (BasicAWSCredentials.
                   (:access-key cred)
                   (:secret-key cred)))
                 (AmazonIdentityManagementClient.))]
    (if (:endpoint cred) (.setEndpoint client (:endpoint cred)))
    client))

(def ^{:private true}
  iam-client
  (memoize iam-client*))

;;
;; convert object graphs to clojure maps
;;

(defprotocol ^{:no-doc true} Mappable
  "Convert a value into a Clojure map."
  (^{:no-doc true} to-map [x] "Return a map of the value."))

(extend-protocol Mappable nil (to-map [_] nil))


;;
;; convert clojure maps to object graphs

(defn- keyword-to-method
  "Convert a dashed keyword to a CamelCase method name"
  [kw]
  (apply str (map string/capitalize (string/split (name kw) #"-"))))

(defn set-fields
  "Use a map of params to call setters on a Java object"
  [obj params]
  (doseq [[k v] params]
    (let [method-name (str "set" (keyword-to-method k))
          method (first (clojure.lang.Reflector/getMethods (.getClass obj) 1 method-name false))
          arg-type (first (.getParameterTypes method))
          arg (if (= arg-type java.lang.Integer) (Integer. v) v)]
      (clojure.lang.Reflector/invokeInstanceMember method-name obj arg)))
  obj)

(declare mapper)

(defn map->ObjectGraph
  "Transform the map of params to a graph of AWS SDK objects"
  [params]
  (let [keys (keys params)]
    (zipmap keys (map #((mapper %) (params %)) keys))))

(defmacro mapper->
  "Creates a function that invokes set-fields on a new object of type
   with mapped parameters."
  [type]
  `(fn [~'params] (set-fields (new ~type) (map->ObjectGraph ~'params))))

(defn- mapper
  ""
  [key]
  (let [mappers {}]
    (if (contains? mappers key) (mapper key) identity)))

;;
;; exceptions
;;

(extend-protocol Mappable
  AmazonServiceException
  (to-map [e]
    {:error-code   (.getErrorCode e)
     :error-type   (.name (.getErrorType e))
     :service-name (.getServiceName e)
     :status-code  (.getStatusCode e)
     :message      (.getMessage e)}))

(defn decode-exception
  "Returns a Clojure containing the details of an AmazonServiceException"
  [exception]
  (to-map exception))


