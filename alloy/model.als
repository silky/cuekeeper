/* Copyright (C) 2015, Thomas Leonard
 * See the README file for details. */

// A Node is roughly a filename (e.g. "db/<uuid>" or "contact/<uuid>")
// Since the same Node may appear in different revisions with
// different values, its fields are defined as part of Rev.
abstract sig Node {}
sig Contact extends Node {}
sig Context extends Node {}

// Area, Project and Action (APA) nodes can change type, so we model
// them as a single sig with a type field (stored in the Rev).
sig APA extends Node {}

abstract sig Type {}
one sig Area extends Type {}
one sig Project extends Type {}
one sig Action extends Type {}

// States for actions. Only WaitingForContact and Done are interesting
// for checking purposes, so everything else is bundled into NextEtc.
abstract sig AState {}
one sig NextEtc, WaitingForContact, Done extends AState {}

// A single Git revision, containing the state of all nodes at that
// point.
sig Rev {
  // Each of these corresponds to a file in the store:
  my_apa : set APA,		/* Files in the 'db' dirctory */
  contacts : set Contact,	/* Files in 'contacts' directory */
  contexts : set Context,	/* Files in 'contexts' directory */

  // These are generated automatically to help with the model checking:
  my_nodes : set Node,
  areas : set my_nodes,
  projects : set my_nodes,
  actions : set my_nodes,

  // These are read from inside the files.
  // References are stored in the source file, so we can be sure the
  // domain of each relation is in my_nodes. However, we can't be sure
  // the target will exist.
  parent : my_apa ->lone APA,
  contact : my_apa ->lone Contact,
  context : actions ->lone Context,
  type : my_apa ->one Type,
  astate : actions ->lone AState,

  // These are calculated from the previous values
  roots : set my_apa,			/* We scan [parent] to find this */
  child_nodes : my_apa lone-> APA,
} {
  // We calculate these from the parent relation
  roots = { a : my_apa | no a.parent }
  child_nodes = ~parent

  // These are only for the modelling:
  my_nodes = my_apa + contacts + contexts
  areas = {x : my_apa | x.type = Area }
  projects = {x : my_apa | x.type = Project }
  actions = {x : my_apa | x.type = Action }
}

/* This corresponds to the checks performed by Ck_rev.
 * Anything that calls [error] should have a corresponding check here. */
pred valid [r : Rev] {
  // No unreachable nodes - TODO
  r.my_apa = (r.roots).*(r.child_nodes)
  all a : r.my_apa {
    // No cycles - TODO
    a not in a.^(r.child_nodes)
    // Actions have no children
    a.(r.type) = Action => no a.(r.child_nodes)
    // Projects only have project and action children
    a.(r.type) = Project => {
      all c : a.(r.child_nodes) { c.(r.type) in Project + Action }
    }
    // Referenced nodes exist
    a.(r.parent) in r.my_apa
    a.(r.context) in r.contexts
    a.(r.contact) in r.contacts
  }
  all a : r.actions {
    a.(r.astate) = WaitingForContact => some a.(r.contact)
  }
  // TODO - done projects have only done actions?
}

pred bad_parents [ p : APA ->lone APA, type : APA ->lone Type ] {
  // Contains a cycle
  (some n : p.univ | n in n.^p) ||
  // An action is a parent of something
  (some n : univ.p | n.type = Action) ||
  // An area has a project parent
  (some area : type.Area, project : type.Project | area -> project in p)
}

// Check whether [result] could be the result of merging [a] and [b]
// with common ancestor [base].
pred merge [base, a, b, result : Rev] {
  // For each set, we have an upper and lower bound.
  // The upper bound is (things added by A) + (things added by B) + (things in common)
  // The lower bound is (things added by A or B) + (things in common)
  result.my_apa in (a.my_apa - base.my_apa) + (b.my_apa - base.my_apa) + (a.my_apa & b.my_apa)
  (b.my_apa - base.my_apa) +
  (a.my_apa - base.my_apa) +
  (a.my_apa & b.my_apa)
    in result.my_apa

  result.contacts in (a.contacts - base.contacts) + (b.contacts - base.contacts) + (a.contacts & b.contacts)
  (b.contacts - base.contacts) +
  (a.contacts - base.contacts) +
  (a.contacts & b.contacts)
    in result.contacts

  result.contexts in (a.contexts - base.contexts) + (b.contexts - base.contexts) + (a.contexts & b.contexts)
  (b.contexts - base.contexts) +
  (a.contexts - base.contexts) +
  (a.contexts & b.contexts)
    in result.contexts

  // For the relations, we must also ensure that we keep any target referred to in a link:
  result.contact in (a.contact - base.contact) + (b.contact - base.contact) + (a.contact & b.contact)
  (b.contact - base.contact) +
  (a.contact - base.contact) +
  (a.contact & b.contact)
    in result.contact
  univ.(result.contact) in result.contacts
  // If a resulting action is waiting for a contact, it must have one of the input actions' contacts.
  all act : result.astate.WaitingForContact {
    (some act.(a.contact) && act.(result.contact) = act.(a.contact)) ||
    (some act.(b.contact) && act.(result.contact) = act.(b.contact))
  }

  result.context in (a.context - base.context) + (b.context - base.context) + (a.context & b.context)
  (a.context - (b.context - base.context)) +
  (b.context - (a.context - base.context)) +
  (a.context & b.context)
    in result.context
  univ.(result.context) in result.contexts

  result.parent in (a.parent - base.parent) + (b.parent - base.parent) + (a.parent & b.parent)
  bad_parents [(a.parent - base.parent) + (b.parent - base.parent) + (a.parent & b.parent), result.type] => {
    // If combining a and b might give a cycle then allow removing any edges necessary to fix it.
    // Also, remove edges to actions and from projects to areas.
    not bad_parents [result.parent, result.type]
  } else {
    // Otherwise, must include all common and added edges, as usual
    (b.parent - base.parent) +
    (a.parent - base.parent) +
    (a.parent & b.parent)
      in result.parent
  }

  result.type in (a.type - base.type) + (b.type - base.type) + (a.type & b.type)
  (b.type - base.type) +
  (a.type - base.type) +
  (a.type & b.type)
    in result.type
  
  result.astate in (a.astate - base.astate) + (b.astate - base.astate) + (a.astate & b.astate)
  (b.astate - base.astate) +
  (a.astate - base.astate) +
  (a.astate & b.astate)
    in result.astate
}

pred equal [a, b : Rev] {
    a.parent = b.parent
    a.my_nodes = b.my_nodes
    a.context = b.context
    a.contact = b.contact
    a.type = b.type
    a.astate = b.astate
}
