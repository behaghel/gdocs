# `gdocs`: Google Docs integration for Emacs org-mode

`gdocs` provides bidirectional synchronization between org-mode files and Google Docs. Open, edit, create, and push Google Docs entirely from within Emacs, using org-mode as the native editing format.

## Overview

The package addresses three main workflows:

- **Publish org to Google Docs.** Create a Google Doc from an existing org file to share with collaborators. The two files remain in sync: edits to either propagate to the other.
- **Read and edit shared Google Docs.** Open a Google Doc shared by someone else as an org-mode buffer in Emacs. Edit without leaving Emacs.
- **Import existing Google Docs.** Import old Google Docs into org files. The files remain synchronized, but you may later choose to unlink or delete the Google Doc.

Under the hood, `gdocs` handles OAuth 2.0 authentication with multi-account support, bidirectional format conversion through an intermediate representation, incremental diffing via a longest-common-subsequence algorithm so only changes are sent over the wire, and a side-by-side conflict resolution UI when local and remote edits collide. All API operations are fully asynchronous via `plz.el`, so Emacs never blocks during network requests.

The `gdocs-mode` minor mode activates automatically on linked buffers, providing auto-push on save, a modeline sync indicator, and keybindings under the `C-c g` prefix for push, pull, status, and browser access.

## Installation

`gdocs` requires Emacs 29.1 or later.

### package-vc (built-in since Emacs 30)

```emacs-lisp
(use-package gdocs
  :vc (:url "https://github.com/benthamite/gdocs"))
```

### Elpaca

```emacs-lisp
(use-package gdocs
  :ensure (:host github :repo "benthamite/gdocs"))
```

### straight.el

```emacs-lisp
(use-package gdocs
  :straight (:host github :repo "benthamite/gdocs"))
```

### Dependencies

- [plz](https://github.com/alphapapa/plz.el) (0.7+) — asynchronous HTTP requests
- [dash](https://github.com/magnars/dash.el) (2.19+) — list manipulation utilities
- [s](https://github.com/magnars/s.el) (1.13+) — string manipulation utilities
- org (9.6+) — built into Emacs

## Quick start

1. Obtain OAuth credentials:

   1. Go to the [Google Cloud Console](https://console.cloud.google.com/) and sign in.
   2. Create a new project (or select an existing one).
   3. Go to **APIs & Services > Library** and enable the **Google Docs API** and the **Google Drive API**.
   4. Go to **APIs & Services > OAuth consent screen**. Select **External** as the user type. Fill in the required fields (app name, your email). On the **Scopes** page, add `https://www.googleapis.com/auth/documents` and `https://www.googleapis.com/auth/drive`. On the **Test users** page, add your own email address.
   5. Go to **APIs & Services > Credentials**. Click **Create Credentials > OAuth client ID**. Select **Desktop app**, enter a name, and click **Create**. Copy the **Client ID** and **Client secret**.

2. Configure your credentials:

   ```emacs-lisp
   (setopt gdocs-accounts
           '(("personal" . ((client-id . "YOUR-CLIENT-ID")
                            (client-secret . "YOUR-CLIENT-SECRET")))))
   ```

3. Authenticate: `M-x gdocs-authenticate`. Your browser will open to Google's consent screen. Grant access and the tokens will be stored locally.

4. Open an existing Google Doc: `M-x gdocs-open`.

5. Or create a new Google Doc from an org file: `M-x gdocs-create`.

Once a buffer is linked, changes are pushed to Google Docs on every save (when `gdocs-auto-push-on-save` is non-nil), and you can pull remote changes at any time with `M-x gdocs-pull`.

## Pull preservation and safety

Full replacement during an ordinary pull preserves the local Org preamble:
metadata values, keyword spelling, property data and leading local comments.
`gdocs-preamble.el` locates the raw boundary without converting document content;
body-owned affiliated attributes remain with the body. Without local metadata,
the rendered remote preamble is retained.

`gdocs-sync-safety.el` owns semantic inventories and validation policy. The sync
orchestrator retains fetch, conflict handling, validation invocation, backup and
replacement. Remote list additions are accepted when existing structural counts
remain intact; lost nesting, task state, blocks and other protected structure
still block replacement. Parsing does not run user Org-mode hooks. File mtime
is not proof that local content is unchanged: hash/shadow checks remain the
source of that decision.

## Semantic encapsulation

Generic conversion/diff engines own traversal, sequence comparison, indexing and
request assembly—not individual feature policy. Adding or modifying a semantic
feature keeps its recognition, identity, normalization, model lengths, addressing
and request rules in the feature's own modules. Moving only request construction
while leaving feature branches in the engine does not satisfy this boundary.

`gdocs-diff.el` depends on a semantic-neutral contract, not concrete feature
modules. `gdocs-diff-semantics.el` defines three ordinary Emacs hooks and explicitly
composes implementations:

- Element descriptors supply content identity and body-model length.
- Run descriptors supply semantic text/fingerprints, whitespace policy and an
  optional reference-request emitter.
- Request plans supply body operations and requests that precede body mutation.

`gdocs-diff-footnotes.el` implements all footnote diff policy through those
contracts, including zero-width references, native-segment matching and ordering.
There is no runtime discovery, declaration DSL or custom feature registry. New
extension points require a concrete behavior that the existing contract cannot
express. Architecture tests enforce the boundary and exercise an unrelated
synthetic semantic kind so the contract is not a renamed feature-specific API.

## Shared text and paragraph primitives

`gdocs-convert-text.el` owns run construction, literal text concatenation,
source soft-wrap normalization, returned soft-break decoding and UTF-16 text
coordinates. `gdocs-convert-paragraphs.el` composes it for paragraph construction
and literal paragraph insertion, including the terminal newline. Both load
without Org, the conversion facade, styles, feature modules or transport.
Existing constructor/helper names remain available through the facade.

These primitives accept already resolved text and formatting data. They do not
recognize native objects, choose IDs, resolve styles or interpret feature
metadata. Source soft wraps become spaces only through the source-text helper;
literal constructors preserve authored newlines. Paragraph insertion accepts a
rendered string, not unresolved semantic runs. Body, heading, list and block
publishing compose this same literal insertion envelope; feature-specific
requests and configurable styling remain outside it. Footnotes and status
conversion require their shared text dependency directly.

## Bidirectional paragraph conversion

`gdocs-convert-paragraph-codec.el` owns plain Org paragraph run decoding,
plain/native paragraph IR assembly, alignment decoding and paragraph text
rendering. Generic inline conversion and ID allocation are explicit document
services; the codec does not require the conversion facade or interpret feature
kinds. Feature renderers receive the paragraph and decoded text, returning a
string when handled or nil otherwise. Composition is explicit and ordered,
without a registry or discovery layer.

The facade composes paragraph adapters rather than recognizing their syntax:

- `gdocs-convert-images.el` claims standalone source images before inline parsing.
- `gdocs-convert-org-blocks.el` owns paragraph-level code/block syntax and rendering.
- `gdocs-convert-lists.el` owns native list paragraph roles, nesting/task state,
  counters and Org list rendering. List roles take precedence over heading styles.
- `gdocs-convert-headings.el` owns native heading/title roles, heading emphasis
  normalization and Org heading/title rendering.
- Native footnotes reuse paragraph run decoding after their feature-owned
  single-paragraph/text-only validation; segment addressing remains footnote policy.
- Status conversion owns removal of inline-only status markers from paragraph
  annotations; those markers remain attached to their semantic runs.

Each paragraph's inline content is decoded once. The plain codec does not choose
feature roles, logical presentation, remote accounts or transport behavior.
Document-wide feature assembly, inline feature conversion and publication request
planning are separate from this paragraph conversion boundary.

## Bidirectional inline conversion

`gdocs-convert-inline-codec.el` owns ordinary Org emphasis/link-description
conversion, native rich-text decoding, Org rendering, run coalescing and edge
whitespace normalization. It loads without the facade, feature modules, logical
style policy or transport. Document link resolution remains an explicit facade
service; this codec neither discovers documents nor selects accounts.

The facade explicitly composes source-object adapters, native-element adapters,
semantic rendering and run policies. Status, dates and footnotes own their
recognition, metadata and normalization rules. Code-like Org objects and native
monospace recognition belong to the block/code adapter. Status owns marker
coalescing completion, unsupported dropdown diagnostics and status-specific style
field/marker requests. `gdocs-convert-dates.el` owns active/inactive timestamp
markers, atomic native date runs, one-unit model addressing and mixed native-date
insertion planning. Reference recognition, authored labels, zero-width rendering
and reference-request extension callbacks belong to footnotes.

A run policy is a plain property list: `:atomic` prevents coalescing; `:identity`
preserves semantic identity; `:preserve-whitespace` and `:preserve-terminal`
protect authored boundaries; `:formatted` contributes to formatting detection;
`:model-length` supplies nonliteral native addressing. Feature modules return
only their local consequences, and the facade composes them explicitly. Generic
algorithms inspect these neutral properties, not feature fields or kind names.
There is no feature registry, discovery mechanism or declaration DSL.

Literal links are composed before emphasis so emphasis encloses the complete
link. Native default link underlining is not mistaken for authored underline.
The source adapter preserves post-blank spacing and inherited formatting; native
inline parts retain their grouping token until feature-owned completion runs.

## Logical style resolution

`gdocs-styles.el` contains presentation data only. `gdocs-style-resolve.el`
resolves that data into native paragraph/text fields, borders, colour objects,
field masks and logical-style requests. It loads without Org, conversion, diff,
semantic feature modules, transport or accounts. Configured native parent names
are translated as presentation enums, without loading heading recognition.
Existing constructor and helper names remain available through the facade.

Resolution retains ordered overrides, explicit zero/false values, fallback
fonts/borders, relative and absolute size handling and existing field order.
Missing entries remain distinguishable from explicitly empty overrides, so
feature-owned fallbacks can preserve caller intent. Resolution does not mutate
caller-owned configuration. Logical paragraph ranges exclude
the terminal newline; text ranges preserve supplied body or segment addresses.
The shared text layer owns the literal range constructor.

Conversion still selects semantic roles and assembles publication requests.
Authored inline emphasis, links and colours remain conversion data, not theme
inference. Changing logical presentation does not change either conversion
direction's IR, regenerated Org, semantic diff identity or non-style requests.
Status presentation calls the resolver directly; choosing a status colour and
maintaining its semantic markers remain status responsibilities.

## Document-link resolution

`gdocs-link-resolve.el` owns explicit source-file context, native document IDs,
heading caches, local document-ID lookup, forward link targets and reverse Org
link resolution. It loads without conversion, presentation, diff, transport or
accounts. Existing helper names and dynamically bound context/cache variables
remain available through the facade. Generic inline conversion declares this
lookup service without requiring it or acquiring filesystem responsibilities.

Loading the service does not discover notes. Callers provide directories and
source context; scans inspect immediate Org files only and retain the bounded
file-level property read. Cached Org-ID lookup suppresses broad location rescans.
Metadata parsing, including Org-ID's temporary buffer, inhibits user mode hooks
and Org startup actions. Lookup does not choose accounts or perform network I/O.

Known file/ID/heading links map to document URLs and cached anchors. Unpublished
Org targets remain plain text; ordinary file and web URLs retain their prior
forms. Reverse resolution preserves unknown URLs and relative file links, and
prefers an existing Org ID for a same-document heading. Cache ordering,
normalization and dynamic callback context remain part of link resolution, not
semantic conversion or typography policy.

## List feature ownership

`gdocs-convert-list-source.el` owns Org list and item recognition, top-level
selection, positioned traversal and structural indentation cleanup.
`gdocs-convert-lists.el` owns native roles, list rendering, counters and managed
continuation semantics. `gdocs-convert-list-publish.el` owns native presets,
nesting tabs, group assembly, alternating numbered presets and post-removal
coordinates. Peer callout/task/continuation features supply their own repairs;
publication keeps their established ordering. Existing helper APIs remain
available through explicit module composition.

`gdocs-diff-lists.el` owns list identity and membership, native prefixes,
bullet request emission, preservation of existing list objects and removal of
residual bullets from surviving paragraphs. Full structural replacement does
not run bullet cleanup against the new non-paragraph element. Partial task
state participates in identity, including inside callouts.

The semantic composition module supplies a neutral paragraph descriptor with
`:structure-key`, `:prefix`, `:compatible` and a `:requests` function. A separate
finisher composes content and modification stages. Generic diff compares opaque
structure and measures physical prefixes in UTF-16; it does not inspect list
fields or native bullet types. Structure compatibility gates style-only and
formatting-only shortcuts as well as word-level editing. An unrelated synthetic
paragraph policy exercises this contract without list syntax or a registry.
Semantic request plans compose opaque `:before` and `:after` requests around body
operations; the kernel does not classify their feature or native request types.

Native membership and nesting edits rebuild the affected complete list group
after body edits. Group repairs retain authored text and inline formatting,
consume structural tabs from a zero-indent baseline, and restore continuation
and callout presentation afterward. They do not rebuild unaffected groups.
Regrouping requires a complete body plan; a push containing declined or
comment-protected body operations fails before mutation with an instruction to
push the nesting edit separately, rather than applying final coordinates to a
partially updated document.

Description terms retain their rich inline content as visible `term :: body`
text in ordinary Docs bullet paragraphs. Rendering and reparsing the standard
Org description syntax preserves terms without duplicating them or requiring
hidden term snapshots. Description syntax is not a native Docs control.

## List paragraph fidelity

List counters retain independent state at each nesting level. Returning from a
child list resumes the parent's counter; continuation paragraphs do not count
as new items. Org rendering uses the same counter assignment as native import.
Complete native bullet/list properties determine actual type and nesting ahead
of older list-state snapshots. Native depth includes the list definition's base
indentation across mixed types; paragraph-only presentation does not redefine
it. Incomplete native definitions retain their annotated fallback. Explicit Org item markers determine type within mixed list
containers. Text edits refresh list-state ranges at post-tab coordinates.

`gdocs-convert-lists.el` retains every direct paragraph of a list item, in
source order around child lists. The first paragraph carries the item's list
role. Later paragraphs carry `:list-continuation` with the owning type and level,
not another bullet or a duplicated task state. Org rendering indents these
paragraphs within the original item without making them children of the most
recent nested item. Unsupported body blocks fail before publication rather than
being dropped or lifted out of the item.

Managed continuation paragraphs have versioned named-range annotations. A
zero-width run projects annotation and indentation requests without adding text
or disturbing rich-body UTF-16 coordinates. Native numbered publication keeps
continuations in the enclosing group, then removes their temporary bullets;
they do not split the list into restarting groups. Callout continuations use
full-width presentation; task continuations do not introduce another glyph.
`gdocs-diff-list-continuations.el` owns semantic annotation identity, emitters
and stale-range cleanup. Repeated unchanged pushes do not recreate annotations.

Import requires an identifiable preceding owner at the declared type and level.
Malformed, overlapping, orphaned, segmented, table-contained or
paragraph-spanning continuation annotations fail closed. Unmarked prose is not
inferred to be a continuation from indentation alone. Structural source trimming
respects literal run whitespace and does not accumulate continuation indentation.

## Callout conversion

`gdocs-convert-callouts.el` owns callout syntax and attributes, type/title
validation, body extraction, native label/body/list metadata, grouping, Org
rendering, publication geometry and post-bullet shading repair.
`gdocs-diff-callouts.el` supplies semantic identity and physical body length.
Generic conversion and diff algorithms delegate these policies without
classifying callout kinds. Paragraph construction, transport, accounts and
asynchronous orchestration remain separate.

Callouts retain native paragraph boundaries and begin a new block at every
label, even when adjacent blocks have identical metadata. UTF-8 titles are
URL-decoded as UTF-8. Published labels record their applied prefix so visible
title edits remain meaningful after local presentation changes; ambiguous or
unrecognized label edits fail closed rather than restoring a stale title.
Named-range metadata is bounded by the native UTF-16 name limit.

Versioned list metadata retains list type, nesting and counters without
stripping list-looking authored text. Task completion is not stored there:
managed visible glyphs own it. Org rendering separates lists from subsequent
prose so a paragraph cannot become a task continuation on reparse. Unsupported
nested callout list shading fails before publication instead of flattening it.
Callout list chrome preserves ordinary hanging columns and compensates the
left border padding so the shaded surface keeps the same edge as prose.
Chrome runs after bullet cleanup and continuation/task indentation repairs.
Configured prose gaps use internal padding within the shaded surface; list
contents remain compact. Outer margins remain separate paragraph spacing.

## Managed task glyphs

`gdocs-convert-tasks.el` projects Org `[ ]`, `[X]` and `[-]` task items to
editable Docs text `☐`, `☑` and `◩`, followed by an em space and the rich body.
The `gdocs-task-prefix` logical style defaults to 10-point Noto Sans Symbols 2
and applies only to this two-unit prefix; authored body text retains its own
theme, emphasis and links.
These are text glyphs, not click-to-toggle native controls. Pull reads the
visible glyph as completion state; metadata identifies managed tasks and
nesting, never an authoritative hidden completion snapshot. Unmarked glyph
lookalikes remain ordinary text, and task-body lookalikes remain literal.
The named range anchors the separator after the glyph, so replacing just the
glyph preserves its identity. Missing glyphs, malformed/orphaned annotations,
unsupported segments and table contexts fail closed with actionable errors.

Version-one nesting uses 36-point structural steps. Hanging glyph columns are
18 points before their body columns, with wrapped text and continuations aligned
to the body. The prefix avoids browser-dependent tab stops; the read-only
`tabStops` field is not sent in update requests. Legacy space- and tab-separated tasks remain readable and
retain their observed physical prefix during diffing, allowing a one-time
upgrade without body or completion loss. Task planning clears native bullet
membership and repairs indentation after adjacent native list operations.
Rich-body spans and markers start after the two-unit glyph/separator prefix.
`gdocs-diff-tasks.el` projects semantic task paragraphs before generic diffing,
owns prefix descriptors and removes stale range annotations before edits.
Changed prefixes are annotated once; no-op/repeated pushes emit no requests.
List-owned contextual spacing distinguishes list boundaries from compact item
contents. Modification finishers restore that spacing after native named-style
updates, without applying named styles again or changing authored text.

Native checkbox completion is not exposed by the documented Docs API. Known
native checkbox forms and legacy-marked task controls are rejected rather than
silently reset. An unspecified glyph also occurs in ordinary native bullets,
so it cannot prove checkbox semantics: unmarked native checklists cannot be
automatically recognized or faithfully imported. Use managed glyph tasks for
synchronized state, or republish authoritative Org to establish that format;
never infer completion from strike-through or an old metadata snapshot.

## Centered prose conversion

`gdocs-convert-centers.el` owns centered source recognition, paragraph/block
identity, native range validation, grouping, Org rendering and publication.
Rich prose retains emphasis, links, Unicode and paragraph boundaries. Distinct
blocks remain distinct, including empty blocks. Soft source wraps remain prose;
centered prose does not acquire verse's literal-line semantics.

Versioned whole-paragraph annotations preserve block identity without storing
stale text. Native text and paragraph insertions are authoritative. Import
requires marked paragraphs to remain centered, text-only and unlisted; partial
or overlapping ranges, unsupported provenance or unsupported content stop with
an actionable diagnostic rather than silently changing meaning. Unsupported
nested source blocks and semantic objects stop before publication. Unmarked
native centered paragraphs retain the existing single-block Org rendering;
adjacency alone does not establish a shared authored block.

`gdocs-diff-centers.el` supplies neutral identity and UTF-16 geometry descriptors,
including the generated outside separator. Repeated pushes are no-ops and edits
preserve following content. Configurable logical center roles own presentation;
the generic diff algorithm and shared codecs do not recognize centered blocks.

## Verse block conversion

`gdocs-convert-verse.el` owns verse source recognition, hard-line conversion,
native block identity/grouping, Org rendering and publication. Verse lines
preserve authored indentation, internal/trailing spaces, blank stanza lines,
ordinary emphasis, links and Unicode; they are not soft-wrapped prose.
Configurable logical verse styles remain presentation rather than authored data.

Native verse annotations identify whole physical line groups, including blank
lines and their terminal newlines. An identified placeholder distinguishes an
empty block from an authored blank line. Native text and inserted lines remain
authoritative; the annotation stores no stale text or line-count snapshot.
Import uses authoritative native ranges and block identity, not capitalization
or paragraph indentation. Unsupported native
objects and legacy markers without a supported structural version fail closed
rather than guessing the missing line structure. Unsupported source semantic
objects and verses nested inside other blocks likewise block before remote
mutation.

`gdocs-diff-verse.el` owns verse identity and UTF-16 geometry through neutral
descriptors. Repeated pushes are no-ops; edits preserve following document
content and rich run boundaries. The conversion facade only composes feature
adapters, and the shared codecs accept literal-whitespace policy without
recognizing verse.

## Quote block conversion

`gdocs-convert-quotes.el` owns source/context quote recognition, paragraph
extraction, native quote markers, grouping, visual-italic cleanup, Org rendering
and publication planning. Contextual Org paragraph adapters delegate quote
policy to this feature rather than interpreting it inside the generic codec.
Quote identity and read-side geometry belong to `gdocs-diff-quotes.el`, exposed
through neutral element descriptors; identity includes style revision and rich
paragraph run formatting/text.

Native grouping preserves authoritative ranges, native paragraph boundaries and
block IDs. It does not infer paragraph structure from capitalization. Authored
italic emphasis is independent of quote presentation: displayed italic is the
exclusive-or of authored italic and the quote style's italic base. Publication
records the applied base in each quote marker and explicitly writes both italic
and upright run ranges. Import reverses that recorded mapping, not the current
local theme, and normalizes whitespace in authored-style space without mutating
input runs. Missing or unsupported style provenance blocks quote import with an
actionable diagnostic; authoritative Org can republish the document to establish
provenance instead of guessing or erasing emphasis.

Publication emits shared paragraph plans with quote-specific logical line roles
and semantic markers, then one unmarked separator paragraph. Body length counts
paragraph terminal newlines and that separator. Empty quote publication includes
an empty content paragraph and the separator.

Shared paragraph construction supplies empty-paragraph recognition and neutral separator
append/consume operations; quote and callout features decide when to invoke
those operations. Callouts do not depend on quote policy. Logical presentation,
transport, accounts and asynchronous orchestration remain separate. Existing
helper APIs remain available through the facade.

## Source and example block conversion

`gdocs-convert-code-blocks.el` owns source/example syntax, literal lines,
language metadata, native marker grouping, Org rendering and publication
planning. Source markers group consecutive paragraphs only when both language
and block ID match; example markers group by block ID. Grouping retains authoritative document ranges.
Legacy unmarked all-code paragraphs group into Org example blocks only when
normal, unlisted, unaligned and unmarked. The feature supplies a plain grouping
descriptor to the facade; generic traversal does not inspect code runs or
assemble block syntax.

Literal block text preserves trailing spaces and authored empty lines; only the
structural final newline is removed when constructing its line list. Native
paragraph decoding removes one terminator, not arbitrary trailing whitespace.
Literal block text does not reinterpret Org emphasis, links or status syntax.
Publication emits one shared paragraph plan per line, preserving code run
properties, semantic markers and first/line/last/single logical style roles.
Empty blocks still publish a terminal newline. UTF-16 body lengths count each
literal line plus its newline. Shared paragraph construction owns only the
neutral line-position role constructor and opaque metadata assembly; code,
quote and callout features choose their own roles and markers.

`gdocs-diff-code-blocks.el` supplies neutral identity/length descriptors. Source
identity includes language, style revision and lines; example identity includes
lines. Generic diff does not classify these block kinds. Inline/contextual Org
adapters compose code-block services without moving quotes, callouts or list
policy into this feature. Existing helper names remain available through the
facade. Presentation configuration, transport, accounts and asynchronous
orchestration remain separate responsibilities.

## Inline source fidelity

Inline source IR preserves language, header arguments and literal code as one
semantic object. Native named ranges carry language/header metadata at exact
UTF-16 body ranges; import restores those ranges even when native text runs
have been split or merged. Diff identity includes the language/header data and
formatting span lengths. Changed or deleted inline source removes stale native
range IDs before applying edits, without duplicating replacement annotations.

Source and native metadata that cannot be represented faithfully fail before
publication or source replacement. Inline source inside tables or footnotes,
empty or multiline native ranges, overlapping semantic markers, discontinuous
ranges, malformed language/header data and excessive native range-name length
produce actionable diagnostics rather than plain-code fallbacks.

## Image conversion and geometry

`gdocs-convert-images.el` owns standalone source-image recognition, native
inline-image decoding, source/caption markers, caption attachment, Org rendering
and publication request geometry. Native traversal composes its element adapter
in document order with other features. Shared document services supply IDs and
marker-range selection; shared text and logical-style services supply literal
insertion, UTF-16 lengths and caption presentation. Image modules do not upload,
fetch files, select accounts or orchestrate asynchronous callbacks.

Source markers encode authored paths as URL-escaped UTF-8; decoding restores
Unicode filenames. Rendering prefers the authored path, then source URI,
content URI and remote object ID. Marked caption paragraphs attach only to an
immediately preceding image and extend its authoritative document range.
Requests insert the image, source marker, visible caption, logical caption
styles and caption marker in order. Missing fetchable URIs produce no insertion
or index advance, leaving publication safety/enrichment to adapter preflight.

`gdocs-diff-images.el` supplies image identity and one-unit native fallback length
through neutral element descriptors. Authored source paths and captions take
precedence over transient object IDs/URIs for identity; authoritative returned
ranges cover attached captions. Generic diff does not classify images or choose
their identity/addressing policy. Existing helper names and the semantic caption
style-name option remain available through the facade.

## Table conversion and geometry

`gdocs-convert-tables.el` owns Org/native table conversion, Org rendering,
cell addressing, structural lengths, proportional widths and request planning.
Cells compose shared inline conversion and native paragraph decoding. Source
horizontal-rule rows are not content rows; multi-row Org rendering places a
separator after the first row. Authored cell emphasis and links are reapplied
following inherited-format resets. Status conversion owns rejection of status
labels inside source and native tables.

Insertion preserves the paragraph at the insertion point, so its structural
span is one unit longer than the read-side table body. Cell text and style
ranges use UTF-16 units; column width proportions use character counts as a
visual proxy. Column geometry uses 468pt available width and a 36pt minimum,
capped by the available width per column. Requests create the table, set column
widths, then populate nonempty cells last-to-first to keep insertion indices
stable. Empty cells still contribute structural markers and terminal newlines.

`gdocs-diff-tables.el` supplies table identity including row/cell shape and rich
cell formatting spans, plus read-side model length, through neutral descriptors. Generic diff algorithms do not
classify tables or calculate table geometry. Table width repair pairs local
content with fetched native table locations, skipping mismatched column counts;
its pure planner lives with table conversion. Sync owns fetching, callbacks,
account/source context and push completion, not column geometry. Table modules
do not discover files, select accounts or perform network I/O. Existing private
helper names remain available through the facade.

## Native footnote conversion

`gdocs-convert-footnotes.el` owns footnote definition/run conversion, native
segment import and segment-local text/style request planning. Transport and
asynchronous native-reference creation remain adapter responsibilities. The
conversion facade loads this module alongside headings and status conversion.

Native footnote IR carries its authored `:label`, rich `:contents`, and remote
`:segment-id` / `:segment-start` addressing metadata. Addressing metadata does not
change semantic equality. Native label annotations resolve into the reference
label; they do not become opaque metadata on its containing paragraph.
Footnote-only text or emphasis/link changes target that existing segment;
unchanged notes produce no requests. Existing segment
updates precede body edits that may remove native references. The segment's
terminal newline is not deleted.

The native mapping preserves a single paragraph of ordinary rich text. Native
dates, nested references, status labels, paragraph breaks and non-text segment
objects fail closed rather than being flattened. Adapters can use
`gdocs-convert-footnote-body-runs` for preflight,
`gdocs-convert-footnote-body-requests` for newly created segments and
`gdocs-convert-footnote-label-requests` to preserve authored labels. While the
native reference text seam is active, definitions remain in semantic IR for
diffing but are not emitted as literal document-body text.

## Documentation

For a comprehensive description of all user options, commands, and functions, see the [manual](https://stafforini.com/notes/gdocs/).

## Roadmap

### Phase 2: comments and suggestions

- [ ] Comments side buffer for viewing and managing Google Docs comments (add, reply, resolve, delete)
- [ ] Suggesting mode — toggle between direct editing and suggestions, with changes sent as Google Docs suggestions rather than direct edits
- [ ] Accept/reject suggestions from other collaborators
- [ ] Periodic polling for remote changes with configurable interval

### Phase 3: Drive browser and export

- [ ] Google Drive browser — a dired-like buffer for browsing, searching, and managing files in Google Drive
- [ ] Org export backend (`C-c C-e g d`) — export to Google Docs via the standard org export dispatcher

## License

[GPL-3.0](LICENSE)
