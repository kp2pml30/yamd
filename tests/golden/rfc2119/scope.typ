The key words #metadata(none)<def-must>#emph[MUST], #metadata(none)<def-must-not>#emph[MUST NOT], REQUIRED, SHALL, SHALL NOT, #metadata(none)<def-should>#emph[SHOULD], #metadata(none)<def-should-not>#emph[SHOULD NOT], RECOMMENDED, NOT RECOMMENDED, #metadata(none)<def-may>#emph[MAY], and OPTIONAL in this document are to be interpreted as described in RFC 2119.

Even when the FIRST reference to a keyword occurs inside an \#xref-scope
(so the collect-time registration runs under that scope), the RFC keywords are
a global vocabulary and resolve everywhere.

Inside the scope, the server #metadata(none)<ref-1>#link(<def-must>)[#yamd-ref(class: "xref-ref")[MUST]] validate the token.

#metadata(none)<def-1-token>#emph[token]

Refer to the #metadata(none)<ref-2>#link(<def-1-token>)[#yamd-ref(class: "xref-ref")[token]] here.

Back at root, the client #metadata(none)<ref-3>#link(<def-should>)[#yamd-ref(class: "xref-ref")[SHOULD]] retry; the scoped token def is
NOT visible here, but the RFC keywords still are: it #metadata(none)<ref-4>#link(<def-may>)[#yamd-ref(class: "xref-ref")[MAY]] cache.
