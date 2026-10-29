# Cureus submission guide

Journal: Cureus (https://www.cureus.com), free standard publication (do not
select the paid Preferred Editing option).
Article type: **Original Article**. Specialty: **Epidemiology/Public Health**
(secondary: Healthcare Technology).

## Files in this folder

| File | Use |
|---|---|
| `Manuscript_Cureus.docx` | Copy each section into the matching box of the Cureus editor |
| `Figure1.png`, `Figure2.png` | Upload as figures (TIFF and PDF versions are here too) |
| `../submission/Additional_file_1-3_*.xlsx` | Upload as appendices, if the form allows; otherwise point to GitHub |

Co-authors' emails are kept outside this public repository, in
`bdhs-promo/cureus-authors.md` on your computer.

Rebuild the Word file after any change to `../manuscript.Rmd`:

```r
rmarkdown::render("paper/manuscript.Rmd", output_format = "word_document",
                  params = list(submission = TRUE, figure_dir = "cureus", journal = "cureus"),
                  output_file = "cureus/Manuscript_Cureus.docx")
```

## Editor boxes

| Cureus box | From `Manuscript_Cureus.docx` |
|---|---|
| Title | Reproducing official estimates from the Bangladesh Demographic and Health Survey, 1993-2022: Application of bdhs R package |
| Abstract | The structured abstract (233 words) |
| Keywords | Demographic and Health Surveys; Bangladesh; complex survey design; reproducibility; validation; synthetic data; health inequality; R |
| Introduction, Materials & Methods, Results, Discussion, Conclusions | The sections with the same names |
| Tables 1-8 | Rebuild with the Cureus table tool by copying the rows from Word |
| Figures 1-2 | Upload the PNG files; paste the captions from "Figure legends" |
| References | Add with the reference tool (paste each DOI where there is one); keep the order 1-21 |

## Authors (in this order)

1. Arman Hossen: Department of Statistics, Miami University, Oxford, Ohio, USA (corresponding author)
2. Ashiqur Rahman Rony: Department of Statistics, Government Titumir College, Dhaka, Bangladesh; Data Analytics, Development Research Initiative (dRi), Dhaka, Bangladesh
3. Abida Sultana Asha: Dr. Bing Zhang Department of Statistics, College of Arts and Sciences, University of Kentucky, Lexington, Kentucky, USA
4. Kazi Sabbir Ahmad Nahin: Dr. Bing Zhang Department of Statistics, College of Arts and Sciences, University of Kentucky, Lexington, Kentucky, USA; Department of Quantitative Sciences, International University of Business Agriculture and Technology, Dhaka, Bangladesh

## Form answers

**Human subjects:** Consent was obtained or waived by all participants in
this study. The original surveys were approved by the ICF Institutional
Review Board and the Bangladesh Medical Research Council, and all respondents
gave informed consent. This study is a secondary analysis of de-identified
data released by The DHS Program, and no further ethical approval was
required.

**Animal subjects:** All authors have confirmed that this study did not
involve animal subjects or tissue.

**Conflicts of interest:**
- Payment/services info: All authors have declared that no financial support
  was received from any organization for the submitted work.
- Financial relationships: All authors have declared that they have no
  financial relationships at present or within the previous three years with
  any organizations that might have an interest in the submitted work.
- Other relationships: All authors have declared that there are no other
  relationships or activities that could appear to have influenced the
  submitted work.

**Author contributions** (Cureus categories, based on the manuscript's
contribution statement):

| Category | Authors |
|---|---|
| Concept and design | Arman Hossen |
| Acquisition, analysis, or interpretation of data | Arman Hossen, Ashiqur Rahman Rony, Abida Sultana Asha, Kazi Sabbir Ahmad Nahin |
| Drafting of the manuscript | Arman Hossen, Kazi Sabbir Ahmad Nahin |
| Critical review of the manuscript for important intellectual content | Ashiqur Rahman Rony, Abida Sultana Asha, Kazi Sabbir Ahmad Nahin |
| Statistical analysis | Arman Hossen |

Leave "Supervision" empty unless one of you supervised the work.

**Acknowledgements:** The authors thank NIPORT, ICF and The DHS Program for
conducting the surveys and making the data available. An AI assistant
(Claude, Anthropic) was used to help write and test software code and to
draft parts of the manuscript. The authors reviewed and verified all code,
analyses and text and take full responsibility for the content of the
article.

**Data availability:** The bdhs package, analysis scripts and all aggregate
results are available at https://github.com/armanhossen-am/bdhs (MIT
licence). BDHS microdata are available from The DHS Program to registered
users (https://dhsprogram.com); the authors are not permitted to share them.

**Prior publication or related work:** A short software paper describing the
bdhs package has been submitted to the Journal of Open Source Software. It
does not report the analyses in this manuscript.

## Before you submit

- Ask Abida Sultana Asha for her email, and all co-authors for their ORCID iDs.
- Send the final `Manuscript_Cureus.docx` to all co-authors for approval.
- Check the abstract word limit Cureus shows in its editor.
- Prepare 5-10 suggested reviewers (name, institution, email), none from your
  own institutions or recent co-authors.
