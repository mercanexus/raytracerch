(ns raytracerch.core
  (:use clojure.test)
  (:gen-class))

(def TX 0)
(def TY 1)
(def TZ 2)
(def TW 3)
(def EPSILON 0.00001)

(defn flpequal
  [a b]
  (< (java.lang.Math/abs (- a b)) EPSILON))

(defn tupequal
  [a1 a2]
  (every? true? (map flpequal a1 a2)))
(defn a-point?
  "Returns true if the tuple is a point."
  [tuple]
  (flpequal (tuple TW) 1.0))

(defn a-vector?
  "Returns true if the tuple is a vector."
  [tuple]
  (flpequal (tuple TW) 0.0))

(defn a-point
  "Returns a point."
  [x y z]
  [x y z 1.0])

(defn a-vector
  "Returns a vector."
  [x y z]
  [x y z 0.0])

(defn add-tuples
  [a1 a2]
  (vec (map + a1 a2)))

(defn subtract-tuples
  [a1 a2]
  (vec (map - a1 a2)))

(defn negate-tuple
  [a1]
  (map - a1))

(defn scalar-mul
  [tuple x]
  (vec (map (fn [n] (* n x)) tuple)))

(defn scalar-div
  [tuple x]
  (vec (map (fn [n] (/ n x)) tuple)))

(defn vector-mag
  [v]
  (let [[x y z _] v]
    (java.lang.Math/pow (+ (* x x) (* y y) (* z z)) 0.5)))

(defn normalize
  [v]
  (let [m (vector-mag v)]
    (vec (map (fn [n] (/ n m)) v))))

(defn dot
  "Returns the dot product of two tuples."
  [a b]
  (apply + (map * a b)))

(defn cross
  "Returns the cross product of two vectors."
  [a b]
  (let [[ax ay az _] a
        [bx by bz _] b]
    (a-vector (- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx)))))

(deftest a-point-test
  (testing "A tuple with w=1.0 is a point"
    (is (= true (a-point? [4.3 -4.2 3.1 1.0])))
    (is (= false (a-vector? [4.3 -4.2 3.1 1.0]))))
  (testing "a-point creates tuples with w=1"
    (is (= true (a-point? (a-point 4 -4 3))))))

(deftest a-vector-test
  (testing "A tuple with w=0 is a vector"
    (is (= false (a-point? [4.3 -4.2 3.1 0.0])))
    (is (= true (a-vector? [4.3 -4.2 3.1 0.0]))))
  (testing "a-vector creates tuples with w=0"
    (is (= true (a-vector? (a-vector 4 -4 3))))))

(deftest add-tuples-test
  (testing "Adding two tuples"
    (is (tupequal [1 1 6 1] (add-tuples (a-point 3 -2 5) (a-vector -2 3 1))))))

(deftest subtract-tuples-test
  (testing "Subtracting two points"
    (is (tupequal (a-vector -2 -4 -6) (subtract-tuples (a-point 3 2 1) (a-point 5 6 7))))
    (is (tupequal (a-point -2 -4 -6) (subtract-tuples (a-point 3 2 1) (a-vector 5 6 7))))
    (is (tupequal (a-vector -2 -4 -6) (subtract-tuples (a-vector 3 2 1) (a-vector 5 6 7))))))

(deftest negate-tuple-test
  (testing "Negating a tuple"
    (is (tupequal [-1 2 -3 4] (negate-tuple [1 -2 3 -4])))))

(deftest scalar-mul-test
  (testing "Multiplying a tuple by a scalar"
    (is (tupequal [3.5 -7 10.5 -14] (scalar-mul [1 -2 3 -4] 3.5))))
  (testing "Multiplying a tuple by a fraction"
    (is (tupequal [0.5 -1 1.5 -2] (scalar-mul [1 -2 3 -4] 0.5)))))

(deftest scalar-div-test
  (testing "Dividing a tuple by a scalar"
    (is (tupequal [0.5 -1 1.5 -2] (scalar-div [1 -2 3 -4] 2)))))

(deftest vector-mag-test
  (testing "Computing the magnitude of vector(1 2 3)"
    (is (= true (flpequal 1 (vector-mag (a-vector 1 0 0)))))
    (is (= true (flpequal 1 (vector-mag (a-vector 0 1 0)))))
    (is (= true (flpequal 1 (vector-mag (a-vector 0 0 1)))))
    (is (= (java.lang.Math/pow 14 0.5) (vector-mag (a-vector 1 2 3))))
    (is (= (java.lang.Math/pow 14 0.5) (vector-mag (a-vector -1 -2 -3))))))

(deftest normalize-test
  (testing "Normalizing vector(4 0 0) gives (1 0 0)"
    (is (tupequal (a-vector 1 0 0) (normalize (a-vector 4 0 0))))
    (is (tupequal (a-vector 0.26726 0.53452 0.80178) (normalize (a-vector 1 2 3))))
    (is (= true (flpequal 1.0 (vector-mag (normalize (a-vector 1 2 3))))))))

(deftest dot-test
  (testing "The dot product of two tuples"
    (is (flpequal 20 (dot (a-vector 1 2 3) (a-vector 2 3 4))))))

(deftest cross-test
  (testing "The cross product of two vectors"
    (is  (tupequal (cross (a-vector 1 2 3) (a-vector 2 3 4)) (a-vector -1 2 -1)))
    (is  (tupequal (cross (a-vector 2 3 4) (a-vector 1 2 3)) (a-vector 1 -2 1)))))
