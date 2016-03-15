(ns structured-data)

(defn do-a-thing [x]
  (let [doubled-x (+ x x)])
  (Math/pow (+ x x) (+ x x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)))


(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1))
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (if (== (width rectangle) (height rectangle)) true false))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x3 y3] point]
    (if (and (<= x1 x3 x2) (<= y1 y3 y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
    (if (and (contains-point? outer [x1 y1])
             (contains-point? outer [x2 y2])) true false)))

(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors [china]})
(def wild-seed {:title "Wild Seed", :authors [octavia]})
(def embassytown {:title "Embassytown", :authors [china]})
(def little-schemer {:title "The Little Schemer"
                     :authors [friedman, felleisen]})
(def books [cities, wild-seed, embassytown, little-schemer])

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (< 1 (author-count book)) true false))

(defn add-author [book new-author]
     (assoc book :authors (conj (get book :authors) new-author))
  )

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [ second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))



(defn stars [n]
  (apply str (repeat n "*")))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false)
  )

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (= (count (set a-seq)) (count a-seq)) false true)
  )

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
 (contains? (:authors (old-book->new-book book)) author))

(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        years (str " (" (:birth-year author) " - " (:death-year author) ")")
        ]
    (str name (if (:birth-year author) years ""))))


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))


(defn book->string [book]
  (apply str (:title book) ", written by "
         (authors->string (:authors book))))


(defn books->string [books]
  (apply str
         (if (== (count books) 0) (str "No books.")
                                  (if (== 1 (count books))
                                    (str (count books) " book. ")
                                    (str (count books) " books. ")
                                    ))
         (map book->string books)))

(count [little-schemer, cities, wild-seed])
(books->string [])
(books->string [cities])
(books->string [little-schemer, cities, wild-seed])

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))

(books-by-author china books)
(books-by-author octavia books)

(defn author-by-name [name authors]
  (first (filter (fn [author] (= name (:name author))) authors)))

(def authors #{china, felleisen, octavia, friedman})

(author-by-name "Octavia E. Butler" authors)
(author-by-name "Octavia E. Butler" #{felleisen, friedman})
(author-by-name "George R. R. Martin" authors)


(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors))

(living-authors #{china, felleisen})

(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
(def christopher {:name "Christopher Tolkien" :birth-year 1924})
(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

(def silmarillion {:title "Silmarillion"
                   :authors #{jrrtolkien, christopher, kay}})

(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})
(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})


(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(has-a-living-author? silmarillion)
(has-a-living-author? wild-seed)
(has-a-living-author? little-schemer)
(has-a-living-author? cities)
(has-a-living-author? deus-irae)

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

(books-by-living-authors books)
(books-by-living-authors (concat books [deus-irae, silmarillion]))

(take 2
      (filter even?
              (range 10)))

; %________%
