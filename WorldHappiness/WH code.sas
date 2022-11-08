/*******************************************/
/*  Group Project Analysis Code			   */
/*******************************************/

/* create the libname indicator we needed */
LIBNAME mylib "~/MySASData/Final Project";

/*******************************************/
/*  1. Create the Analysis Dataset		   */
/*******************************************/

/* import 2015 dataset */
PROC IMPORT DATAFILE="~/MySASData/Final Project/2015.csv"
DBMS=CSV
OUT=mylib.score_2015 REPLACE;
GUESSINGROWS=5000;
RUN;

/* import 2019 dataset */
PROC IMPORT DATAFILE="~/MySASData/Final Project/2019.csv"
DBMS=CSV
OUT=mylib.score_2019 REPLACE;
GUESSINGROWS=5000;
RUN;

/**************************************************/
/* 2. Clean up the data                           */
/**************************************************/

/* sort, merge, and rename the datasets, drop those variables we don't need */
DATA mylib.score_2019;
	SET mylib.score_2019;
	RENAME 'Country or region'N = Country;
RUN;

PROC SORT DATA=mylib.score_2019 OUT=mylib.score_2019_sorted;
	BY Country;
RUN;
PROC SORT DATA=mylib.score_2015 OUT=mylib.score_2015_sorted;
	BY Country;
RUN;

DATA mylib.score_merged;
	MERGE mylib.score_2015_sorted (DROP = 'Standard Error'N Family 'Dystopia Residual'N
								   RENAME=('Happiness Rank'N=rank_2015 'Happiness Score'N=score_2015
								   		   'Economy (GDP per Capita)'N=Economy_2015 'Health (Life Expectancy)'N=Health_2015
								   		   Freedom=Freedom_2015 'Trust (Government Corruption)'N=Trust_2015 Generosity=Generosity_2015))
		  mylib.score_2019_sorted (DROP = 'Social Support'N
		  						   RENAME=('Overall rank'N=rank_2019 Score=score_2019 'GDP per capita'N=Economy_2019 
		  						   		   'Healthy life expectancy'N=Health_2019 'Freedom to make life choices'N=Freedom_2019
		  						   		   Generosity=Generosity_2019 'Perceptions of corruption'N=Trust_2019));
	BY Country;
RUN;

/* create new variables needed */
DATA mylib.score_merged;
	SET mylib.score_merged;
	Score_change = score_2019 - score_2015;
	rank_change = rank_2019 - rank_2015;
	length happiness_level_2015 $ 6;
	length happiness_level_2019 $ 6;
	IF (score_2015<=4 & score_2015>2) THEN happiness_level_2015 = 'Low';
	IF (score_2015<=5 & score_2015>4) THEN happiness_level_2015 = 'Medium';
	IF (score_2015<=6 & score_2015>5) THEN happiness_level_2015 = 'Fair';
	IF (score_2015<8 & score_2015>6) THEN happiness_level_2015 = 'High';
	IF (score_2019<=4 & score_2019>2) THEN happiness_level_2019 = "Low";
	IF (score_2019<=5 & score_2019>4) THEN happiness_level_2019 = "Medium";
	IF (score_2019<=6 & score_2019>5) THEN happiness_level_2019 = "Fair";
	IF (score_2019<8 & score_2019>6) THEN happiness_level_2019 = "High";
RUN;

/* Summary of the dataset */
PROC CONTENTS DATA=mylib.score_merged;
TITLE "Contents of Complete Data Set Ready for Analysis (Healy and Haocheng Wang)"; 
RUN;

/* Specific question 1 (Healy):  Does the score of countries in Europe generally higher than which of countries in Asia? Does this result change in 2019 compare to 2015?
   Variables:
   1) [new] region2: the general region a country in (for example, summarize all countries in estern Europe, northern Europe
   					 under "Europe").
   2) [new]Score_change: the change in score Between 2015 and 2019 for each country
   3) score_2015: the happiness score of each country in 2015
   4) score_2019: the happiness score of each country in 2019
*/

/* Specific question 2 (Haocheng):  Does the mean health index differ from different happiness scores? Is an individual’s higher happiness score associated with a significantly healthier life? 
   Variables:
   1) [new] happiness_level_2015: ranked happiness level based on the happiness score in 2015
   2) [new] happiness_level_2019: ranked happiness level based on the happiness score in 2019
   3) health_2015: health score of each country in 2015
   4) health_2019: health score of each country in 2019
*/

/*******************************************/
/*  3. Create Table One					   */
/*******************************************/

/* export the table in word version */
ODS WORD FILE="~/MySASData/Final Project/Table_One.docx";
/* export it in a PDF file */
*ODS PDF FILE="~/MySASData/Final Project/Table_One.pdf";
/* create table one by calculating the mean/stddev for continous variables and count/proportion for categorical variable */
TITLE "Table One";
PROC TABULATE DATA=mylib.score_merged;
	VAR rank_2015 score_2015 health_2015
		rank_2019 score_2019 health_2019
		score_change rank_change;
	CLASS Region;
	TABLE ALL='Sample Size'*N=' ' (rank_2015 score_2015 health_2015 rank_2019 score_2019 health_2019
		  						   score_change rank_change)*(MEAN STDDEV)*FORMAT=5.3, (Region=' ' ALL) / BOX="Region";
RUN;
*ODS PDF CLOSE;	

ODS WORD CLOSE;	 

/*******************************************/
/*  4. Create Graphics					   */
/*******************************************/

/* Specific question 1 (Healy):  Does the score of countries in Europe generally higher than which of countries in Asia? Does this result change in 2019 compare to 2015?
   Variables:
   1) [new] region2: the general region a country in (for example, summarize all countries in estern Europe, northern Europe
   					 under "Europe").
   2) [new]Score_change: the change in score Between 2015 and 2019 for each country
   3) score_2015: the happiness score of each country in 2015
   4) score_2019: the happiness score of each country in 2019
*/

/* filter out the countries in Asia and Europe */
DATA mylib.AsiaAndEurope;
	SET mylib.score_merged;
	IF FIND(Region, 'Asia')~=0 OR FIND(Region, 'Europe')~=0 THEN OUTPUT;
RUN;
/* create a new variable that indicate the countries' general region */
DATA mylib.AsiaAndEurope;
	SET mylib.AsiaAndEurope;
	IF FIND(Region, 'Asia')~=0 THEN Region_2 = "Asia";
	IF FIND(Region, 'Europe')~=0 THEN Region_2 = "Europe";
RUN;

/* create boxplot that summarize the mean of the scores for countries in different region */
ODS LISTING GPATH="~/MySASData/Final Project" IMAGE_DPI=300;
ODS GRAPHICS / IMAGENAME="Asia_Europe_Compare";
TITLE "Comparison of Mean Score of Asia and Europe";
PROC SGPLOT DATA=mylib.asiaandeurope;
	VBOX Score_2015 / Group=Region_2;
	VBOX Score_2019 / Group=Region_2 TRANSPARENCY=0.6;
RUN;
ODS LISTING CLOSE;


/* Specific question 2 (Haocheng):  Does the mean health index differ from different happiness scores? Is an individual’s higher happiness score associated with a significantly healthier life? 
   Variables:
   1) [new] happiness_level_2015: ranked happiness level based on the happiness score in 2015
   2) [new] happiness_level_2019: ranked happiness level based on the happiness score in 2019
   3) health_2015: health score of each country in 2015
   4) health_2019: health score of each country in 2019
*/
ODS LISTING GPATH="~/MySASData/Final Project" IMAGE_DPI=300; 
ODS GRAPHICS / RESET OUTPUTFMT=PNG	HEIGHT=4in WIDTH=4in;

/* create boxplot to compare mean health score across different happiness level in 2015*/
ODS GRAPHICS / IMAGENAME="Health Distribution 2015";
TITLE1 "Compare Mean Health Index for Different Happiness Levels in 2015";
TITLE2 "Test the null hypothesis that the mean health index does not differ by happiness level in 2015";
PROC ANOVA DATA=mylib.score_merged;
	CLASS happiness_level_2015;
	MODEL Health_2015 = happiness_level_2015;
	MEANS happiness_level_2015;
RUN;

/* create boxplot to compare mean health score across different happiness level in 2019*/
ODS GRAPHICS / IMAGENAME="Health Distribution 2019";
TITLE1 "Compare Mean Health Index for Different Happiness Levels in 2019";
TITLE2 "Test the null hypothesis that the mean health index does not differ by happiness level in 2019";
PROC ANOVA DATA=mylib.score_merged;
	CLASS happiness_level_2019;
	MODEL Health_2019 = happiness_level_2019;
	MEANS happiness_level_2019; 
RUN;

ODS LISTING CLOSE;

/*******************************************/
/*  5. Preliminary Analysis				   */
/*******************************************/

/* Specific question 1 (Healy):  Does the score of countries in Europe generally higher than which of countries in Asia? Does this result change in 2019 compare to 2015?
   Variables:
   1) [new] region2: the general region a country in (for example, summarize all countries in estern Europe, northern Europe
   					 under "Europe").
   2) [new]Score_change: the change in score Between 2015 and 2019 for each country
   3) score_2015: the happiness score of each country in 2015
   4) score_2019: the happiness score of each country in 2019
*/

ODS LISTING GPATH="~/MySASData/Final Project" IMAGE_DPI=300; 
ODS GRAPHICS / RESET OUTPUTFMT=PNG	HEIGHT=4in WIDTH=4in;

/* compare the 2015 mean score of countries in Europe and Asia */
*ODS TRACE ON;
ODS GRAPHICS / IMAGENAME="qq-plot 2015";
TITLE "Question 1: Compare 2015 Mean Scores of Countries in Europe and Asia ";
PROC TTEST DATA=mylib.asiaandeurope H0=0 SIDE=L;
	CLASS Region_2;
	VAR score_2015;
	ODS OUTPUT ConfLimits=mylib.CI_2015 TTests=mylib.ttest_2015;
RUN;
*ODS TRACE OFF;

/* compare the 2019 mean score of countries in Europe and Asia */
*ODS TRACE ON;
ODS GRAPHICS / IMAGENAME="qq-plot 2019";
TITLE "Question 1: Compare 2019 Mean Scores of Countries in Europe and Asia ";
PROC TTEST DATA=mylib.asiaandeurope H0=0 SIDE=L;
	CLASS Region_2;
	VAR score_2019;
	ODS OUTPUT ConfLimits=mylib.CI_2019 TTests=mylib.ttest_2019;
RUN;
*ODS TRACE OFF;

ODS LISTING CLOSE;

/* Stack the comparison of data from 2015 and 2019 */
DATA mylib.ttest_stacked;
	SET mylib.ttest_2015 mylib.ttest_2019;
	WHERE Variances ~= "Equal";
	DROP Method Variances;
RUN;

DATA mylib.CI_stacked;
	SET mylib.ci_2015 mylib.ci_2019;
	WHERE Method = "Satterthwaite";
	KEEP Variable Method Variances Mean LowerCLMean UpperCLMean;
RUN;

/* Merge them to create a easy-to-read table */
DATA mylib.statistic_merge;
	MERGE mylib.CI_stacked mylib.ttest_stacked;
	BY Variable;
	CI = CATS("(", PUT(LowerCLMean, 8.2), ", ", PUT(UpperCLMean, 8.2), ")");
RUN;

/* Summary Table */

ODS WORD FILE="~/MySASData/Final Project/PreliminaryAnalysis.docx";

TITLE "Question 1: Summary of the T-Test";
PROC PRINT DATA=mylib.statistic_merge LABEL;
	LABEL Mean="Estimate" LowerCLMean="Lower Bound" UpperCLMean="Upper Bound"
		  tValue="T-Value" Probt="P-Value";
	ID Variable;
	VAR Method Mean LowerCLMean UpperCLMean tValue DF Probt CI;
RUN;

ODS WORD CLOSE;


/* Specific question 2 (Haocheng):  Does the mean health index differ from different happiness scores? Is an individual’s higher happiness score associated with a significantly healthier life? 
   Variables:
   1) [new] happiness_level_2015: ranked happiness level based on the happiness score in 2015
   2) [new] happiness_level_2019: ranked happiness level based on the happiness score in 2019
   3) health_2015: health score of each country in 2015
   4) health_2019: health score of each country in 2019
*/
/* Dataset specific to happiness level high and low in 2019*/
DATA work.happiness_highandlow;
	SET mylib.score_merged;
	IF FIND(happiness_level_2019, 'High')~=0 OR FIND(happiness_level_2019, 'Low')~=0 THEN OUTPUT;
RUN;

ODS LISTING GPATH="~/MySASData/Final Project" IMAGE_DPI=300; 
ODS GRAPHICS / RESET OUTPUTFMT=PNG	HEIGHT=4in WIDTH=4in;

/* analyze the health score for different happiness levels in 2019 */
*ODS TRACE ON;
ODS GRAPHICS / IMAGENAME="health-plot 2019";
TITLE "Question 2: Analyze the health score for different happiness levels in 2019 ";
PROC TTEST DATA=work.happiness_highandlow H0=0 SIDE=L;
	CLASS happiness_level_2019;
	VAR health_2019;
	ODS OUTPUT ConfLimits=mylib.healthCI_2019 TTests=mylib.healthttest_2019;
RUN;
*ODS TRACE OFF;






 				
	
