[![DOI](https://zenodo.org/badge/480716989.svg)](https://zenodo.org/badge/latestdoi/480716989)

# Data Privacy Survey

## About
In the second quarter of 2022, Utrecht University (UU) Research Data Management 
Support (RDM Support) sent out a survey among all scientific personnel at Utrecht 
University, and organised one-on-one meetings with a selection of them. The aim 
of these efforts was to investigate 1) How UU researchers currently deal with 
personal data in their research, 2) What challenges they run into when handling 
personal data in research, and 3) How support at UU can improve their services 
concerning personal data in research.

The survey and one-on-one meetings were part of the 
<a href="https://utrechtuniversity.github.io/dataprivacyproject" target="_blank">Data Privacy Project</a>, 
an RDM Support  project to improve information, tools and services surrounding 
personal data in research. 

<a href = "https://utrechtuniversity.github.io/dataprivacysurvey/docs/data-privacy-survey-report.html"><button>Results report</button></a>
<a href = "https://utrechtuniversity.github.io/dataprivacysurvey/docs/data-privacy-survey-recommendations.html"><button>Recommendations report</button></a>

## This repository

This repository contains the documentation, code, fake survey data and the two
reports written about the survey.

**Documentation** can be found in the `documentation` folder of this repository. 
It contains:

- The full survey
<a href="https://utrechtuniversity.github.io/dataprivacysurvey/documentation/survey-questions-qualtrics.pdf" target="_blank">in text</a> and
<a href="https://utrechtuniversity.github.io/dataprivacysurvey/documentation/Data_Privacy_Survey_Qualtrics_export.qsf" target="_blank">as Qualtrics export</a>.
- The 
<a href = "https://utrechtuniversity.github.io/dataprivacysurvey/documentation/survey-questions-qualtrics.pdf"
target = "_blank">full survey</a>.
- The survey's <a href = "https://utrechtuniversity.github.io/dataprivacysurvey/documentation/survey-privacy-statement.pdf" target = "_blank">privacy statement</a>.
- The survey's <a href = "https://utrechtuniversity.github.io/dataprivacysurvey/documentation/survey-data-management-plan.pdf"
target = "_blank">Data Management Plan</a>.
- The <a href="https://github.com/UtrechtUniversity/dataprivacysurvey/blob/main/documentation/meeting-request.md" target="_blank">invitation</a> sent out to researchers to meet one-on-one.
- The <a href="https://github.com/UtrechtUniversity/dataprivacysurvey/blob/main/documentation/codes-open-text-responses-meetings.csv" target="_blank">codes used </a> to score open text responses and meeting notes.
- A <a href = "https://github.com/UtrechtUniversity/dataprivacysurvey/blob/main/documentation/survey-codebook.csv" target = "_blank">codebook</a> of the online survey in .csv format.

**Code** can be found in the <a href="https://github.com/UtrechtUniversity/dataprivacysurvey/tree/main/src" target = "_blank">
src folder</a> of this repository:

- `pseudonymise-data.R` reads in the raw data and cleans it up to create a 
pseudonymised version. It writes the pseudonymised data into the `data/pseud` folder 
(not publicly available). If the cleaned dataset passes the (limited) k-anonymity 
checks, it is written to the `data/processed` folder instead.
- `plot-data.R` contains all code needed to create the Results report.
- `create-codebook.R` contains code to create the codebook.
- `data-privacy-survey-report.Rmd` is the file underlying the published Results 
report. It loads the `plot-data.R` script to create all visualisations.
- `data-privacy-survey-recommendations.Rmd` is the file underlying the 
Recommendations report. 

As the **dataset** contains personal information (demographic information, open text 
responses, email addresses, etc.), and no consent was obtained to share those 
details, we are unable to share the dataset in this repository. Instead, we 
created <a href="https://github.com/UtrechtUniversity/dataprivacysurvey/tree/main/data/processed" target = "_blank">fake data files</a>, 
which can be used to reproduce the Results report. 

The two **reports** written about the survey are the following:

- The <a href = "https://utrechtuniversity.github.io/dataprivacysurvey/docs/data-privacy-survey-report.html">Results report</a> describes 
the methodology and full results, both for the entire Utrecht University as well 
as the separate faculties.
- The <a href = "https://utrechtuniversity.github.io/dataprivacysurvey/docs/data-privacy-survey-recommendations.html">Recommendations report</a> 
summarises the results and provides Recommendations to improve privacy-related 
support for Utrecht University researchers.

## Contact and contribution

For questions about this repository, please contact Utrecht University's <a href = "https://www.uu.nl/en/research/research-data-management/contact-us" target = "_blank">Research Data Management Support</a>, or open an Issue or Pull request in this repository.

## License and citation

This repository is licensed under a CC BY 4.0 license. You can view the <a href= "https://github.com/UtrechtUniversity/dataprivacysurvey/blob/main/LICENSE.md" target = "_blank">license text here</a>.

When using materials from this repository, please cite us as specified in https://doi.org/10.5281/zenodo.7253451.
