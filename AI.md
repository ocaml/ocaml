## Guidelines relevant to AI-assisted contributions

Tools using AI are increasingly used to produce text and code, but verifying the
quality of such output can be difficult.  We ask you, the contributor(s), to be
willing to adapt your workflow accordingly.  The guidelines also apply, or have
a direct counterpart, with entirely human-produced content.

1. We strive to evaluate contributions based on their merit, not their authors.

2. We ask that you have either written, or read and reviewed yourself,
   *every part* of your contribution.  You are responsible for its quality.
   It is strongly recommended that you review a proposed contribution in full
   even if you wrote all of it.

3. If a significant portion of your code, PR description, review comments or
   messages has been AI-generated, this must be disclosed, stating which tool
   was used and for what.  Reviewing AI-produced code can require a different
   approach from human-written code.  You should have an acknowledgement
   anyway even if another *human* contributor did some of the work.

4. Please be mindful that reviewer time is a scarce resource. Contributions
   (whether human- or AI-authored) should strive to conserve it.

5. When you submit code, however it was written, you take responsibility for
   its provenance.  You will need to sign the OCaml contributor licence
   agreement (CLA) for any significant contribution.

6. The following are never acceptable as contributions: low-effort productions
   of code that contributors do not understand; plagiarised works; hallucinated
   security reports; or experiments involving humans without their advance
   consent.

## Guidelines relevant to AI-assisted bug reports

Our advice is to avoid adding LLM-generated contents or explanations to
your initial bug report: a bug report should be to the point and
contain only human-verified information. Adding LLM-generated contents
runs counter to those objectives, even if the generated contents
looks useful to you.

Indeed, judging the quality of a LLM-generated content about an
unknown code base requires a lot of effort. Simultaneously, estimating
the faithfulness of LLM-generated contents without knowing your prior
understanding of the code base, nor your prompt, nor the model context
also requires a lot of effort. Such efforts are in most cases better
spent investigating the bug itself.

Moreover, mixing human and LLM-generated contents in the same report
without a clear separation should be imperatively avoided. Indeed,
the absence of a clear delineation between the two contents makes it
very hard to ascertain the reliability of any part of the mixed
contents.

We also advise some caution when shrinking reproduction case with
LLMs. If you have any doubts about the LLM-shrinked reproduction case,
please report both the original reproduction case and the shrinked
one. In extreme cases, we have witnessed LLMs convincing people to
reduce their bug report to a hallucination.
