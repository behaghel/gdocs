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

`gdocs-diff-tables.el` supplies text-only table identity and read-side model
length through neutral element descriptors. Generic diff algorithms do not
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
change semantic equality. Footnote-only text or emphasis/link changes target
that existing segment; unchanged notes produce no requests. Existing segment
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
