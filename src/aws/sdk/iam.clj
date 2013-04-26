(ns aws.sdk.iam
  "Functions to access the AWS IAM.

  Each function takes a map of credentials as its first argument. The
  credentials map may contain an :access-key key and a :secret-key
  key. It can also contain an optional :endpoint key if you wish to
  specify an API endpoint (i.e. region) other than us-east."

  (:import com.amazonaws.AmazonServiceException
           com.amazonaws.auth.BasicAWSCredentials
           com.amazonaws.services.identitymanagement.AmazonIdentityManagementClient
           com.amazonaws.services.identitymanagement.model.AddRoleToInstanceProfileRequest
           com.amazonaws.services.identitymanagement.model.CreateInstanceProfileRequest
           com.amazonaws.services.identitymanagement.model.CreateRoleRequest
           com.amazonaws.services.identitymanagement.model.DeleteInstanceProfileRequest
           com.amazonaws.services.identitymanagement.model.DeleteRoleRequest
           com.amazonaws.services.identitymanagement.model.DeleteRolePolicyRequest
           com.amazonaws.services.identitymanagement.model.GetInstanceProfileRequest
           com.amazonaws.services.identitymanagement.model.GetRolePolicyRequest
           com.amazonaws.services.identitymanagement.model.GetRolePolicyResult
           com.amazonaws.services.identitymanagement.model.InstanceProfile
           com.amazonaws.services.identitymanagement.model.ListRolePoliciesRequest
           com.amazonaws.services.identitymanagement.model.PutRolePolicyRequest
           com.amazonaws.services.identitymanagement.model.RemoveRoleFromInstanceProfileRequest
           com.amazonaws.services.identitymanagement.model.Role)

  (:require [clojure.string :as string]))


(defn- iam-client*
  "Create an AmazonIdentityManagementClient instance from a map of credentials."
  [cred]
  (let [client (if (and (:access-key cred) (:secret-key cred))
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
;;

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
    (if (contains? mappers key) (mappers key) identity)))

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

;;
;; Roles, Policies, Instance Profiles
;; 

(def ^{:private true} default-role-creation-params {:path "/"
                                                    :assume-role-policy-document "{\"Version\":\"2008-10-17\",\"Statement\":[{\"Effect\":\"Allow\",\"Principal\":{\"Service\":[\"ec2.amazonaws.com\"]},\"Action\":[\"sts:AssumeRole\"]}]}"})

(extend-protocol Mappable
  Role
  (to-map [r]
    {:id                          (.getRoleId r)
     :name                        (.getRoleName r)
     :arn                         (.getArn r)
     :assume-role-policy-document (java.net.URLDecoder/decode (.getAssumeRolePolicyDocument r))
     :created-date                (.getCreateDate r)
     :path                        (.getPath r)})
  GetRolePolicyResult
  (to-map [grpr]
    {:role-name       (.getRoleName grpr)
     :policy-name     (.getPolicyName grpr)
     :policy-document (java.net.URLDecoder/decode (.getPolicyDocument grpr))})
  InstanceProfile
  (to-map [ip]
    {:arn         (.getArn ip)
     :id          (.getInstanceProfileId ip)
     :name        (.getInstanceProfileName ip)
     :create-date (.getCreateDate ip)
     :path        (.getPath ip)
     :roles       (map to-map (.getRoles ip))}))

(defn list-roles 
  "List IAM roles. Warning response may be truncated!

   Example of returned data structure;
   ({:role-id \"AROAIPRNAHWD6N2AFU756\",
     :role-name \"all-aws-role\",
     :arn \"arn:aws:iam::012767801645:role/all-aws-role\",
     :assume-role-policy-document
     \"{\"Version\":\"2008-10-17\",\"Statement\":[{\"Sid\":\"\",\"Effect\":\"Allow\",\"Principal\":{\"Service\":\"ec2.amazonaws.com\"},\"Action\":\"sts:AssumeRole\"}]}\",
     :created-date #<Date Tue Apr 02 14:07:26 JST 2013>,
     :path \"/\"}
    ...)"

  [cred]
  (map to-map (.getRoles (.listRoles (iam-client cred)))))

(defn create-role 
  "Create a new role with the parameters provided, and return the newly created role.
   Default path and assumeRolePolicyDocument used unless specified per what the AWS console provides.

   E.g.:
   (create-role cred {:role-name \"Role-name-here\"})"
  [cred params]
  (to-map (.getRole (.createRole (iam-client cred) ((mapper-> CreateRoleRequest) (merge default-role-creation-params params))))))

(defn delete-role
  "Delete a role by name.

   E.g.:
   (delete-role cred \"role-name\")"
  [cred role-name]
  (.deleteRole (iam-client cred) (doto (DeleteRoleRequest.)
                                   (.setRoleName role-name))))

(defn list-role-policies
  "Returns a vector of the policy names associated with a given role-name.

   E.g.:
   (list-role-policies cred \"role-name\")"
  [cred role-name]
  (vec (.getPolicyNames (.listRolePolicies (iam-client cred) (doto (ListRolePoliciesRequest.) (.setRoleName role-name))))))

(defn get-policy
  "Returns the policy document for a given role-name and policy-name.

   E.g.:
   (get-policy cred {:role-name \"role-name\" :policy-name \"AdminAccess-all-role-name-201204021407\"})

   Structure returned will appear like:
   {:role-name \"all-aws-role\",
    :policy-name \"AdministratorAccess-all-aws-role-201304021407\",
    :policy-document \"{\n  \"Statement\": [\n    {\n      \"Effect\": \"Allow\",\n      \"Action\": \"*\",\n      \"Resource\": \"*\"\n    }\n  ]\n}\"}"
  [cred params]
  (to-map (.getRolePolicy (iam-client cred) ((mapper-> GetRolePolicyRequest) params))))

(defn get-role-policies
  "Returns policies assigned to a given role-name. 
   Structure returned is defined by the output of get-policy.

   E.g.:
   (get-role-policies cred \"role-name\")"
  [cred role-name]
  (->> (list-role-policies cred role-name)
       (map (fn [policy-name] {:role-name role-name 
                               :policy-name policy-name}))
       (map (partial get-policy cred))))

(defn put-role-policy
  "Add a policy to an existing role.
   Note: function naming follows AWS library here ( http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/services/identitymanagement/AmazonIdentityManagementClient.html )

   E.g.:
   (put-role-policy cred {:role-name \"role-name\" 
                          :policy-name \"Soup-for-you\" 
                          :policy-document \"{\"Statement\":[{\"Effect\": \"Allow\", \"Action\": \"*\", \"Resource\": \"*\"}]}\"})"
  [cred params]
  (.putRolePolicy (iam-client cred) ((mapper-> PutRolePolicyRequest) params)))

(defn delete-role-policy
  "Delete a policy from a role by name.

   E.g.:
   (delete-role-policy cred {:role-name \"role-name\" :policy-name \"policy-name\"})"
  [cred {:keys [role-name policy-name]}]
  (.deleteRolePolicy (iam-client cred) (doto (DeleteRolePolicyRequest.)
                                         (.setRoleName role-name)
                                         (.setPolicyName policy-name))))

(defn list-instance-profiles 
  "List all instance profiles.

   E.g.:
   (list-instance-profiles cred)"
  [cred]
  (map to-map (.getInstanceProfiles (.listInstanceProfiles (iam-client cred)))))

(defn get-instance-profile
  "Return the instance profile for the provided instant profile name.

   E.g.:
   (get-instance-profile cred \"instance-profile-name\")"
  [cred profile-name]
  (to-map (.getInstanceProfile (.getInstanceProfile (iam-client cred) (doto (GetInstanceProfileRequest.)
                                                    (.setInstanceProfileName profile-name))))))

(defn create-instance-profile
  "Create a new instance profile with the name provided.
   Optionally the path can be defined as well, otherwise \"/\" is used.
 
   E.g.:
   (create-instance-profile cred \"New-instance-profile-name\")
   or
   (create-instance-profile cred \"New-instance-profile-name\" \"/\")"
  ([cred profile-name] (create-instance-profile cred profile-name "/"))
  ([cred profile-name path] 
     (to-map (.getInstanceProfile (.createInstanceProfile (iam-client cred) (doto (CreateInstanceProfileRequest.)
                                                                              (.setInstanceProfileName profile-name)
                                                                              (.setPath path)))))))

(defn add-role-to-instance-profile 
  "Assign a role to an instance profile.

   E.g.:
   (add-role-to-instance-profile cred \"role-name\" \"instance-profile-name\")"
  [cred role-name instance-profile-name]
  (.addRoleToInstanceProfile (iam-client cred) (doto (AddRoleToInstanceProfileRequest.)
                                                 (.setRoleName role-name)
                                                 (.setInstanceProfileName instance-profile-name))))

(defn remove-role-from-instance-profile
  "Remove a named role from a named instance profile.

   E.g.:
   
"
  [cred role-name instance-profile-name]
  (.removeRoleFromInstanceProfile (iam-client cred) (doto (RemoveRoleFromInstanceProfileRequest.)
                                                      (.setInstanceProfileName instance-profile-name)
                                                      (.setRoleName role-name))))

(defn delete-instance-profile
  "Delete an instance profile by name.

   E.g.:
   (delete-instance-profile cred \"instance-profile-name\")"
  [cred instance-profile-name]
  (.deleteInstanceProfile (iam-client cred) (doto (DeleteInstanceProfileRequest.)
                                              (.setInstanceProfileName instance-profile-name))))

