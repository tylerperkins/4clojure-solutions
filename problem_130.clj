(ns four-clojure.problem_130
  "This is my solution to 4Clojure problem #130, \"Tree reparenting\". See
  http://www.4clojure.com/problem/130
      -- Tyler"

  (:require [clojure.test :refer :all]))


(defn solution
  "Given the name of a node contained in a tree (the given root node),
  successively hoist each node on the path from the root to the named node.
  Since a child of a hoisted node retains its relationship with its parent,
  the result is a new tree with the node as the new root and former ancestors
  now descending to the right. It's as though you had grabbed one node of the
  tree and dragged it up to the root position, leaving all connections intact."
  [the-node-name root]

  (letfn [
           (hoist
             ; Given a parent node and one of its child nodes, return a node
             ; that is the same as the child node, except it has an additional
             ; child as its last. The new child is the same as the former
             ; parent node, except it no longer has the given child node as a
             ; child.
             [[tree-name & children] [name & _ :as node]]

             (let [demoted-tree (cons
                                  tree-name
                                  (remove #(= name (first %)) children))
                   ]
               (concat node [demoted-tree])))

           (path
             ; Generate the path from this-node to the node named
             ; the-node-name. Return it appended to the given path so far.
             [so-far [name & children :as this-node]]

             (let [path-here (conj so-far this-node)]
               (if (= the-node-name name)
                 path-here        ; Found it!
                 ; Else, examine the children of this-node.
                 (some (partial path path-here) children))))
           ]
    (reduce hoist (path [] root))))


(deftest test-solution
  (are [result       name tree] (= result (solution name tree))
        '(n)         'n   '(n)
        '(a (t (e))) 'a   '(t (e) (a))
        '(e (t (a))) 'e   '(a (t (e)))
        '(a (b (c))) 'a   '(c (b (a)))
        '(d (b (c) (e) (a (f (g) (h)))))
                     'd   '(a (b (c) (d) (e)) (f (g) (h)))
        '(c (d) (e) (b (f (g) (h)) (a (i (j (k) (l)) (m (n) (o))))))
                     'c   '(a (b (c (d) (e)) (f (g) (h))) (i (j (k) (l)) (m (n) (o))))
       )
  )

