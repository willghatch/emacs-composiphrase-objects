# composiphrase-objects

This repo has a bunch of elisp with “text object” definitions.
They could be used stand-alone as useful emacs commands for motions, text selection, transposition, etc.
But they also are designed specifically with my [composiphrase](https://github.com/willghatch/emacs-composiphrase) library in mind.
I am in the process of making a demo that puts all of my recent packages together to create a modal editor with a composable editing language.
TODO - link.

These text objects typically include:
- Motion forward/backward explicitly to the object's beginning/end.
- An “expand region” selection function
- Functions to transpose (drag) the object forward or backward.

The more interesting of the text objects form trees.
- These typically respect tree boundaries, making forward/backward motion mean forward/backward among siblings.
- These come with motions to go up to parent, down to first child, etc.
- The selection functions can be called repeatedly to expand to parent regions.
- There are also inner selection functions that select just the children of a tree.
- Each tree has an in-order traversal function.  This was kinda fun to write, but doesn't seem very useful in practice.
- Each tree type has an “anchor point”, typically the start of the tree object, that is the main point for operating on the tree.  If the anchor point is not the beginning, movements default to moving to the anchor point instead of beginning/end.
- Trees should all support tree operations like slurp, barf, promote/unwrap, demote/wrap, transpose-sibling, reorder-ancestors, etc.  In practice I've only implemented some of these on some trees due to time pressure.  I would like to flesh it out over time.
- Note that while some of the trees may seem like they are specific to specific file types or modes, many are usable to varying degrees outside of their specific mode.  Eg. smartparens is useful outside of lisp, even if not as useful as for lisp.  Indentation trees are useful in virtually any programming language.  Outline trees are typically in org-mode, but there is also outline-minor-mode.  Program source code can be embedded in org-mode.  Etc.  So it is often the case that having multiple different tree views of the same text is useful.  (This is one of the reasons why I believe that pure structured editing, where you don't even view the source tree as text anymore, is going to always be inferior to layering structured editing on top of text.)

Not all objects defined here follow all of these rules.

This library is not stable.
This library does not include any good software engineering practices.
This library first accreted slowly as I wrote movements for myself occasionally, typically in a quick and dirty fashion, then rapidly as I decided to implement Composiphrase and related packages.
I quickly wrote a bunch of movements to have them for myself, and to expand the demo of what Composiphrase can do, in terms of making it much easier to learn and use hundreds of editing commands, by using a composable editing language.
I wanted to limit the time I would spend futzing with emacs instead of other “more important” projects, so this is mostly slapdash demo-ware.
That said, I use it.
So feel free to give it a try, just don't expect stability, or for things to work, or to be able to contribute meaningfully (eg. I've written almost no tests!).

# Objects provided

## simple objects

Some objects already exist natively in emacs, but emacs native motions typically move to the end of an object when going forward, and to the beginning of an object when moving backward.
I want to be able to explicitly move to the beginning or end.
So for these objects, I've mostly added those explicit movements, plus selection and transposition functions.

- word
- sentence
- paragraph
- line
- symbol

It also includes a hacky, bad, and buggy implementation of a more vi-like word.
I've been using it for a little while, and I'm undecided as to whether I will keep using it and fix it or switch to using the normal emacs word definition...

## Smartparens (cpo-smartparens)

[Smartparens](https://github.com/Fuco1/smartparens) is an awesome emacs library for structural editing of s-expressions.
It is also very similar to [paredit](http://paredit.org/).
Smartparens has more customizability than paredit, and in particular allows you to define arbitrary delimiters.
So I've been a smartparens user for many years.
This integration adds:

- Movements explicitly to the beginning/end.
- Movements that respect tree bounds, so you don't move past the last sibling.
- Slightly different boundaries for operations.  When point is before an open paren (or before a symbol, or such), operations typically operate on that s-exp that it is before (with some exceptions that operate on the parent when point is on a symbol or other atomic s-exp).  Additionally, when point is immediately after an s-exp (close paren or otherwise), operations operate on the s-exp that is immediately behind point.  When there are sibling s-exps both immediately before and after point (which doesn't happen with typical Lisp style), the sibling after point should be preferred.  IE this implements a consistent anchor point for operations.

Basically it just provides the goodies of smartparens but with some adjustments to standardize with other tree operations in this package.

## Indent Trees (cpo-indent-tree)

Typically when programming, in any language, we use indentation to help us read.
This indentation forms trees, where children are indented more than their parents.
So `cpo-indent-tree` provides movements to move between siblings, up/down the tree, select indent trees (or the children of an indent tree), transpose indent trees, etc.
I find this text object to be very useful for working with programming languages that don't otherwise have good editing support, or when other tooling is broken (crashing or something).
Indent trees work generically and always work.
This is one of the earliest objects that I wrote, so I've actually been using this one for a pretty long time, and I've found it pretty valuable.
It is still missing several operations, though.

## Outline / org-mode (cpo-outline)

The headings in outline-mode or org-mode (or outline-minor-mode) form a tree.
Note that I decided to try to use outline-mode for the implementation, but some things seem broken because of that.
I may switch to using org-mode-based implementations, which seem more reliable.
But I also wanted to use outline-minor-mode, because sometimes I like to use org-mode style heading trees in code comments.
Anyway, this one is particularly sketchy.

## Treesitter (cpo-treesitter-qd)

This provides generic treesitter movements.
They require a treesitter parser to be initialized in the buffer.
This mostly just includes movements, but it also includes `cpo-treesitter-qd-ancestor-reorder` for something like transposing the parent node up the tree (similar to but not quite the same as the smartparens or paredit convolute operation).
This all works great for lisp, and much faster than smartparens for the operations supported.
For other languages it is... iffy.

It is difficult for treesitter operations to be truly generic.
The first difficulty is figuring out which leaf tree nodes are interesting or uninteresting token types.
You can do this OK semi-generically by listing typical programming language keywords and syntax tokens, as the name of the node type for these is typically the literal text of the token.

Some reasons that Lisps allow such great and easy editing functions, such that structured editing functionality was available for lisp many years ago that is still better than anything provided even with smartparens for other languages, are that lisps provide clear “anchor points” due to being fully explicit with s-expression structure, and that s-expressions are extremely regular, so that operations that make sense anywhere in a tree make sense everywhere in a tree.
With typical unenlightened language syntax, trees are highly irregular, so you can't “just” slurp/barf tree siblings, etc.
And there is not necessarily any “anchor point”, where you can say that a position within the text corresponds specifically to one and only one tree node in the concrete syntax tree.

So this quick-and-dirty implementation assumes that the left-most non-interesting leaf node that a non-leaf node has is its anchor point.
So for infix math operators, the operator is the anchor point.
(It's not entirely fair to say that the operator is uninteresting, but do you want to say that the operator corresponds to an identifier-like node for the operator, or to be the one and only anchor point for the infix operation?)

Anyway, I've played with it a little.
It works ok, sometimes.
By which, I mean it is awesome for Lisp, and it works in some positions for eg. Javascript, but also lets me down in places where I want to use it.

In the future I will probably also try to write some language-specific treesitter wrappers that work better.
But that is a lot of extra work for each of these un-fun syntaxes.
Come on, people, learn lisp and make them popular.
Lisp syntax is simply better in almost all situations.
I hated Lisp the first time I used it in college for a class with no guidance on how to write it.
But it only takes a few minutes to learn the indentation rules for writing lisp, and then you realize that it is easy to read as long as it is formatted properly.
And the editing!
Lisp is so much more fun when text editing!
Also macros (particularly with Racket's macro system!) are awesome, and all of you non-lispers are missing out.

But I digress.

## cpo-search-movements

Ok, this one doesn't really fit, it's not a text object at all, in the sense that the others are.

This has some wrappers to make emacs isearch behave more like searching in vim.
IE searching moves specifically to the start of the search pattern, unless you use the function that makes it go explicitly to the end, whether you go forward or backward.

Also it has `cpo-find-char-beginning-in-line-forward` and friends, which are kinda like (but not quite like) Vim's f/t keys.
I am an ex-Vim user, ex-evil-mode user, writing “a new modal text editor” in emacs.
I like this operation, so I wrote it to keep using it.

## future

I intend to write more text object implementations, especially for more trees, especially for trees that already have some emacs library that supports them.
I am confident that I will add XML at some point with nxml, for example.

## TODO

I need to actually document things here.
This readme just gives some vague high-level understanding of the basic ideas of the various text objects.
It doesn't list any APIs.
But there are a lot of them.
See the full composiphrase demo configuration at TODO (I'm still working on it!) to see an example where all of the motions are bound.
