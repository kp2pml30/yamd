// yamd default Typst preamble — REFERENCE asset.
//
// The Typst backend emits document FRAGMENTS: whoever assembles the final .typ
// is responsible for prepending this preamble (or their own). It is not
// injected automatically.
//
// It defines `yamd-ref`, the styling hook the backend wraps every #ref body in:
// the backend renders a cross-reference as
//
//     #link(<def-anchor>)[#yamd-ref(class: "xref-ref api")[virtual machine]]
//
// so the surrounding #link (and yamd's careful same-page-anchor handling) stays
// in the backend, and only the VISIBLE styling lives here — overridable, not
// baked into the content. `class` is the space-separated class list the ref
// carries: always "xref-ref", plus any #xref-scope(ref-class: …) tokens
// (def-scope then ref-scope). Branch on it to style per scope.

#let yamd-ref(body, class: "") = {
  let classes = class.split(" ").filter(c => c != "")
  // Per-scope example: refs tied to an `api` scope render upright small-caps
  // instead of italic. Delete or adapt.
  if "api" in classes {
    smallcaps(body)
  } else {
    // Default presentation of a cross-reference: italic.
    emph(body)
  }
}
