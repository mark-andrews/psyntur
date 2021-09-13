#' Visual versus Verbal Perception and Responses
#' 
#' An experiment studying the interaction between visual versus perception and visual versus verbal responses.
#' 
#' Subjects carried out two kinds of tasks. One task was visual (describing a
#' diagram), and the other was classed as verbal (reading and describing a
#' sentence sentences). They reported the results either by pointing (a "visual"
#' response), or speaking (a verbal response). Time to complete each task was
#' recorded in seconds.
#' 
#' @name vizverb
#' @docType data
#' @format A data frame with 80 observations on the following 5 variables.
#' \describe{ 
#' \item{subject}{Subject identifying number (\code{s1} to \code{s20})}
#' \item{task}{Describe a diagram (`visual`) or a sentence (`verbal`)} 
#' \item{response}{Point response (`visual`) or say response (`verbal`)} 
#' \item{time}{Response time (in seconds)} 
#' }
#' @source This data set was taken from the
#'   \href{https://cran.r-project.org/package=Stat2Data}{`Stats2Data`
#'   R package}. From the description in that package, the original data appear
#'   to have been collected in a Mount Holyoke College psychology class based
#'   replication of an experiment by Brooks, L., R. (1968) "Spatial and verbal
#'   components of the act of recall," Canadian J. Psych. V 22, pp. 349 - 368.
#' @keywords datasets
"vizverb"


#' Faithfulness from a Photo?
#'
#' Ratings from a facial photo and actual faithfulness.
#'
#' College students were asked to look at a photograph of an opposite-sex adult
#' face and to rate the person, on a scale from 1 (low) to 10 (high), for
#' attractiveness. They were also asked to rate trustworthiness, faithfulness,
#' and sexual dimorphism (i.e., how masculine a male face is and how feminine a
#' female face is). Overall, 68 students (34 males and 34 females) rated 170
#' faces (88 men and 82 women).
#'
#' @name faithfulfaces
#' @docType data
#' @format A data frame with 170 observations on the following 7 variables.
#'   \describe{
#'   \item{sex_dimorph}{Rating of sexual dimorphism (masculinity for males, femininity for females)} 
#'   \item{attractive}{Rating of attractiveness}
#'   \item{cheater}{Was the face subject unfaithful to a partner?}
#'   \item{trustworthy}{Rating of trustworthiness} 
#'   \item{faithful}{Rating of faithfulness} 
#'   \item{face_sex}{Sex of face (female or male)}
#'   \item{rater_sex}{Sex of rater (female or male)} 
#'   }
#' @source This data set was taken from the
#'   \href{https://cran.r-project.org/package=Stat2Data}{`Stats2Data`
#'    R package}. From the description in that package, the original is based on
#'   G. Rhodes et al. (2012), "Women can judge sexual unfaithfulness from
#'   unfamiliar men's faces," Biology Letters, November 2012. All of the 68
#'   raters were heterosexual Caucasians, as were the 170 persons who were
#'   rated. (We have deleted 3 subjects with missing values and 16 subjects who
#'   were over age 35.)
#' @keywords datasets
"faithfulfaces"


#' Age of Onset of Schizophrenia Data
#' 
#' 
#' Data on sex differences in the age of onset of schizophrenia.
#' 
#' 
#' A sex difference in the age of onset of schizophrenia was noted by Kraepelin
#' (1919). Subsequently epidemiological studies of the disorder have
#' consistently shown an earlier onset in men than in women. One model that has
#' been suggested to explain this observed difference is known as the subtype
#' model which postulates two type of schizophrenia, one characterised by early
#' onset, typical symptoms and poor premorbid competence, and the other by late
#' onset, atypical symptoms, and good premorbid competence.  The early onset
#' type is assumed to be largely a disorder of men and the late onset largely a
#' disorder of women.
#' 
#' @name schizophrenia
#' @docType data
#' @format A data frame with 251 observations on the following 2 variables.
#' \describe{ 
#'    \item{age}{Age at the time of diagnosis.}
#'    \item{gender}{A categorical variable with values `female` and `male`}
#' }
#' @source This data set was taken from the
#'   \href{https://cran.r-project.org/package=HSAUR}{`HSAUR` R
#'   package}. From the description in that package, the original is E.
#'   Kraepelin (1919), \emph{Dementia Praecox and Paraphrenia}.  Livingstone,
#'   Edinburgh.
#' @keywords datasets
"schizophrenia"


#' Paired sleep data
#' 
#' Data which show the effect of two soporific drugs (increase in hours of
#' sleep compared to control) on 10 patients.
#' 
#' @name pairedsleep
#' @docType data
#' @format A data frame with 10 observations on the following 3 variables.
#' \describe{ 
#'    \item{ID}{The patient ID.}
#'    \item{y1}{The increase in hours, relative to control, for drug 1.}
#'    \item{y2}{The increase in hours, relative to control, for drug 2.}
#' }
#' @source This data is a transformed version of [datasets::sleep].
#' @keywords datasets
"pairedsleep"


#' Anthropometric data from US Army Personnel
#' 
#' Data on the height, weight, handedness from men and women of different ages and different races.
#' 
#' @name ansur
#' @docType data
#' @format A data frame with 6068 observations from 9 variables.
#' \describe{ 
#'    \item{subjectid}{Unique ID of the person}
#'    \item{gender}{Binary variable indicating the subject's sex: `male` or `female`.}
#'    \item{height}{Height in centimeters.}
#'    \item{weight}{Weight in kilograms.}
#'    \item{handedness}{Categorical variable indicating if the person is left, or right handed, or both.}
#'    \item{age}{Age in years}
#'    \item{race}{Race, with categories like `white`, `black`, `hispanic`.}
#'    \item{height_tercile}{The tercile of the person's height.}
#'    \item{age_tercile}{The tercile of the person's weight.}
#' }
#' @source This data is a transformed version of data sets obtained the \href{https://www.openlab.psu.edu/ansur2/}{Anthropometric Survey of US Army Personnel (ANSUR 2 or ANSUR II)}.
"ansur"

#'Self-Esteem Score Data for One-way Repeated Measures ANOVA
#'
#'@description The dataset contains 10 individuals' self-esteem score on three time points
#'  during a specific diet to determine whether their self-esteem improved.
#'
#'  One-way repeated measures ANOVA can be performed in order to determine the
#'  effect of time on the self-esteem score.
#'  
#'  This data set was taken from the
#'   \href{https://cran.r-project.org/package=datarium}{`datarium` R
#'   package}.
#'@name selfesteem
#'@docType data
#'@usage data("selfesteem")
#'@format A data frame with 10 rows and 4 columns.
#' @examples
#' data(selfesteem)
#' selfesteem
NULL

#'Job Satisfaction Data for Two-Way ANOVA
#'
#'@description
#'  Contains the job satisfaction score organized by gender and education level.
#'  This data set was taken from the
#'   \href{https://cran.r-project.org/package=datarium}{`datarium` R
#'   package}.
#'@name jobsatisfaction
#'@docType data
#'@usage data("jobsatisfaction")
#'@format A data frame with 58 rows and 3 columns.
#' @examples
#' data(jobsatisfaction)
#' jobsatisfaction
NULL


#'Self Esteem Score Data for Two-way Repeated Measures ANOVA
#'
#'@description Data are the self esteem score of 12 individuals enrolled in 2
#'  successive short-term trials (4 weeks) - control (placebo) and special diet
#'  trials.
#'
#'  The self esteem score was recorded at three time points: at the beginning
#'  (t1), midway (t2) and at the end (t3) of the trials.
#'
#'  The same 12 participants are enrolled in the two different trials with
#'  enough time between trials.
#'
#'  Two-way repeated measures ANOVA can be performed in order to determine
#'  whether there is interaction between time and treatment on the self
#'  esteem score.
#'  
#'  This data set was taken from the
#'   \href{https://cran.r-project.org/package=datarium}{`datarium` R
#'   package}.
#'@name selfesteem2
#'@docType data
#'@usage data("selfesteem2")
#'@format A data frame with 24 rows and 5 columns.
#' @examples
#' data(selfesteem2)
#' selfesteem2
NULL

#'Self Esteem Score Data for Two-way Repeated Measures ANOVA: Long format
#'
#'@description Data are the self esteem score of 12 individuals enrolled in 2
#'  successive short-term trials (4 weeks) - control (placebo) and special diet
#'  trials.
#'
#'  The self esteem score was recorded at three time points: at the beginning
#'  (t1), midway (t2) and at the end (t3) of the trials.
#'
#'  The same 12 participants are enrolled in the two different trials with
#'  enough time between trials.
#'
#'  Two-way repeated measures ANOVA can be performed in order to determine
#'  whether there is interaction between time and treatment on the self
#'  esteem score.
#'  
#'  This data set was converted from the `selfesteem2` data taken from the
#'   \href{https://cran.r-project.org/package=datarium}{`datarium` R
#'   package}.
#'@name selfesteem2_long
#'@docType data
#'@usage data("selfesteem2_long")
#'@format A data frame with 72 rows and 4 columns.
#' \describe{ 
#'    \item{id}{Unique ID of the person}
#'    \item{treatment}{Binary variable indicating the treatment condition: `Diet` or `ctr`.}
#'    \item{time}{A categorical variable indicating the time of measurement: beginning (`t1`), midway (`t2`) and at the end (`t3`) }
#'    \item{score}{Self-esteem score}
#' }
#' @examples
#' data(selfesteem2_long)
#' selfesteem2_long
"selfesteem2_long"


#' Psychometrics raw data from testing or demo purposes
#' 
#' @description Typical psychometrics raw data files have multiple psychometric 
#' variables (scales), each with multiple constituent items.
#' In this data set, there are three psychometric variables, each with 10 constituent items.
#' The variables can be labelled `x`, `y`, and `z`.
#' The constituent items of `x`, `y` and `z` are `x_1, x_2 ... x_10`, 
#' `y_1, y_2 ... y_10`, `z_1, z_2 ... z_10`, respectively.
#' @name test_psychometrics
#' @docType data
#' @usage data('test_psychometrics')
#' @format A data frame with 44 rows and 30 columns
#' @examples 
#' data(test_psychometrics)
#' test_psychometrics
"test_psychometrics"