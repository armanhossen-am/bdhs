# Statistical Journal of the IAOS: submission guide

Publisher: Sage. **No fee to submit or publish.** Optional open access costs
extra; choose the standard (subscription) option. Reviewers are chosen by the
journal; you do not suggest reviewers or advisers.

Submission site: https://mc.manuscriptcentral.com/sjiaos
Guidelines: https://journals.sagepub.com/author-instructions/SJI

## Files to upload (all in this folder)

| File | Upload as |
|---|---|
| `Manuscript_anonymized.docx` | Main document (anonymized: no author names) |
| `Title_page.docx` | Title page (not sent to reviewers). **Add your phone number first.** |
| `Cover_letter.docx` | Cover letter (or paste its text) |
| `Figure1.tiff`, `Figure2.tiff` | Figures |
| `Supplemental_file_1-3_*.xlsx` | Supplemental material |

Rebuild the manuscript after any change to `../manuscript.Rmd`:

```r
rmarkdown::render("paper/manuscript.Rmd", output_format = "word_document",
                  params = list(submission = TRUE, figure_dir = "sjiaos", journal = "sjiaos"),
                  output_file = "sjiaos/Manuscript_anonymized.docx")
```

## Steps

1. Open https://mc.manuscriptcentral.com/sjiaos and click **Create An Account**
   (or log in). Link your ORCID in your account settings.
2. Go to the **Author Center** and click **Begin Submission**.
3. **Type:** Original Research Article.
4. **Title:** Reproducing official estimates from the Bangladesh Demographic and Health Survey, 1993-2022: Application of bdhs R package
5. **Abstract:** copy the abstract (179 words) from `Manuscript_anonymized.docx`.
6. **Keywords:** Demographic and Health Surveys; official statistics; reproducibility; complex survey design; synthetic data; Bangladesh.
7. **Authors:** add all four in order, exactly as on the title page, with
   emails (in `bdhs-promo/cureus-authors.md` on your computer). Mark yourself
   as corresponding author.
8. **Upload files** as listed above, choosing the matching file type for each.
9. **Questions:**
   - Conflicts of interest: none (use the wording on the title page).
   - Funding: none.
   - Previously published / under consideration elsewhere: No. Mention the JOSS software paper where the form asks about related work.
   - Preprint: No (unless you post one).
   - Counts: about 3,500 words, 8 tables, 2 figures.
10. **Review the PDF proof** the system builds, then click **Submit**.

## After submitting

- You will get an email with a manuscript number.
- The editor first checks format and scope; it may come back for small fixes.
- Peer review is single-anonymized and usually takes a few months.
