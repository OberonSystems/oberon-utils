(ns oberon.utils-test
  (:require [clojure.test :refer :all]
            [oberon.utils :refer :all]))

(deftest test-suffix-keys
  (is (= (suffix-key :one       :test)    :one-test))
  (is (= (suffix-key :one    :ns/test)    :one-test))
  (is (= (suffix-key :ns/one    :test) :ns/one-test)))

(deftest test-prefix-unprefix-keys
  (is (= (prefix-key   :one      :test) :test-one))
  (is (= (unprefix-key :test-one :test) :one))

  (is (= (prefix-key   :n1/one         :test)         :n1/test-one))
  (is (= (prefix-key      :one      :n2/test)         :n2/test-one))
  (is (= (prefix-key   :n1/one      :n2/test)         :n2/test-one))
  (is (= (prefix-key      :one         :test :ns :n3) :n3/test-one))

  (is (= (unprefix-key :n1/test-one    :test)         :n1/one))
  (is (= (unprefix-key    :test-one :n2/test)         :n2/one))
  (is (= (unprefix-key :n1/test-one :n2/test)         :n2/one))
  (is (= (unprefix-key    :test-one    :test :ns :n3) :n3/one))

  (is (= (prefix-keys [:one :two :three]             :test) [:test-one :test-two :test-three]))
  (is (= (prefix-keys [:one :two :three]          :n1/test) [:n1/test-one :n1/test-two :n1/test-three]))
  (is (= (prefix-keys [:n1/one :n1/two :n1/three]    :test) [:n1/test-one :n1/test-two :n1/test-three]))
  (is (= (prefix-keys [:n1/one :n1/two :n1/three] :n2/test) [:n2/test-one :n2/test-two :n2/test-three]))
  (is (= (prefix-keys [:n1/one :n1/two :n1/three] :n2/test :ns :n3) [:n3/test-one :n3/test-two :n3/test-three]))

  (is (= (unprefix-keys [:test-one :test-two :test-three]             :test) [:one :two :three]))
  (is (= (unprefix-keys [:test-one :test-two :test-three]          :n1/test) [:n1/one :n1/two :n1/three]))
  (is (= (unprefix-keys [:n1/test-one :n1/test-two :n1/test-three]    :test) [:n1/one :n1/two :n1/three]))
  (is (= (unprefix-keys [:n1/test-one :n1/test-two :n1/test-three] :n2/test) [:n2/one :n2/two :n2/three]))
  (is (= (unprefix-keys [:n1/test-one :n1/test-two :n1/test-three] :n2/test :ns :n3) [:n3/one :n3/two :n3/three])))

(deftest test-nest-unnest-map
  (is (= (nest-map {:test-one   1
                    :test-two   2
                    :test-three 3}
                   :test)
         {:test {:one   1
                 :two   2
                 :three 3}}))

  (is (= (unnest-map {:test {:one   1
                             :two   2
                             :three 3}}
                     :test)
         {:test-one   1
          :test-two   2
          :test-three 3})))
