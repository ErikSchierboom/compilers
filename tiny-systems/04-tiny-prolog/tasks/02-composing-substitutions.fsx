// ----------------------------------------------------------------------------
// 02 - Composing and applying substitutions
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Term>) term = 
  // TODO: Replace variables in 'term' for which there is a
  // replacement specified by 'subst.[var]' with the replacement.
  // You can assume the terms in 'subst' do not contain
  // any of the variables that we want to replace.
  failwith "not implemented"


let substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) = 
  // TODO: Apply the substitution 'newSubst' to all the terms 
  // in the existing substitiution 'subst'. (We represent one 
  // as a map and the other as a list of pairs, which is a bit 
  // inelegant, but it makes calling this function easier later.)
  failwith "not implemented"


let substituteTerms (subst:Map<string, Term>) (terms:list<Term>) = 
  // TODO: Apply substitution 'subst' to all the terms in 'terms'
  failwith "not implemented"


let rec unifyLists l1 l2 = 
  // TODO: Modify the implementation to use 'substituteTerms' and 'substituteSubst'.
  //
  // Let's say that your code calls 'unify h1 h2' to get a substitution 's1'
  // and then it calls 'unifyLists t1 t2' to get a substitution 's2' and then
  // it returns a concatentated list 's1 @ s2'. Modify the code so that:
  //
  // (1) The substitution 's1' is aplied to 't1' and 't2' before calling 'unifyLists'
  // (2) The substitution 's2' is applied to all terms in substitution 's1' before returning
  //
  // You can look at your ML type inference code. The structure is very similar! 
  failwith "implemented in step 1"

and unify t1 t2 = 
  failwith "implemented in step 1"

// ----------------------------------------------------------------------------
// Advanced unification tests requiring correct substitution
// ----------------------------------------------------------------------------

// Rquires (1)
// Example: loves(narcissus, narcissus) ~ loves(X, X)
// Returns: [ X -> narcissus ]
unify
  (Predicate("loves", [Atom("narcissus"); Atom("narcissus")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))

// Requires (1)
// Example: loves(odysseus, penelope) ~ loves(X, X)
// Returns: None (cannot unify)
unify
  (Predicate("loves", [Atom("odysseus"); Atom("penelope")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))

// Requires (1)
// Example: add(zero, succ(zero)) ~ add(Y, succ(Y))
// Returns: [ Y -> zero ]
unify
  (Predicate("add", [Atom("zero"); Predicate("succ", [Atom("zero")])]))
  (Predicate("add", [Variable("Y"); Predicate("succ", [Variable("Y")])]))

// Requires (2)
// Example: loves(X, narcissus) ~ loves(Y, X)
// Returns: [ X -> narcissus; Y -> narcissus ]
unify
  (Predicate("loves", [Variable("X"); Atom("narcissus")]))
  (Predicate("loves", [Variable("Y"); Variable("X")]))

// Requires (2)
// Example: add(succ(X), X) ~ add(Y, succ(Z))
// Returns: [ X -> succ(Z); Y -> succ(succ(Z)) ]
unify
  (Predicate("add", 
      [ Predicate("succ", [Variable("X")]); 
        Variable("X") ]))
  (Predicate("add", 
      [ Variable("Y"); 
        Predicate("succ", [Variable("Z")]) ]))

