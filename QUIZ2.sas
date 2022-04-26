******Directions in words we can find ****************
straight:
i.   Horizontal forward
ii.  Horizontal reverse
iii. Vertical down
iv.  Vertical reverse
crosses/diagonals:
v.    Left to right cross forward
vi.   Left to right cross Reverse
vii.  Right to left cross forward
viii. Right to left cross Reverse

***************************************************************;
libname quiz1_a "/home/u50017288/sasuser.v94/quiz1_a" ;

options noquotelenmax; 

data one;
INPUT _1$ _2$ _3$ _4$ _5$ _6$ _7$ _8$ _9$ _10$ _11$ _12$ _13$ _14$ 
	  _15$ _16$ _17$ _18$ _19$ _20$ _21$ _22$ _23$ _24$;
CARDS;
G B X D Z C U S K M L I N F L U E N Z A C T T Z 
L J D M O H S U R I V T S E R O F H A M R A B Y 
A E R Q B L T R H H U N U P U S I L I N P Y S J 
N N G K O A Y I P I S G O N O R R H O E A M O S 
D O D I T M P V A V I A N I N F L U E N Z A C S 
U O O Y U Y H D P Q F X O N A H E X N W K H R G 
L V N G L D O R I Z Q O H I B I P T Q S S X Y A 
A A O D I I I O I M R T O N F Y R T B M U M P S 
R H V K S A D N U H T H O D P T O A Q P Q U T S 
F E A S M A L L P O X N S O B P S R L J B A O I 
E P N T V W W S M S R L J E F O Y X C A L J S S 
V A O Z C H X M E T Z B T N L M R J L C M U P O
E T S O D O V O B P P A E Z Q S L N O A O P O R 
R I I O S O F I O O C A W N N X A C E U S B R I 
I T S X E P X A L S E I B A R P T E B X J H I P 
L I M K J I X X A E H U S G X L B D M Q K H D S 
U S X O P N E K C I N C Z P E R T U S S I S I O 
R A N V N G A S I S O L L E C U R B H H I V O T 
H R W M C C G A S T R O E H T E R I T I S F S P 
Y E X D S D T T D J P H T H E R I A P N V D I E 
U L Z U E U Y X A L L E B U R V Q L D Z P J S L 
F D I T M G Q W E U G N E D A N T H R A X B O M 
R H Q H X H G I A L Z V Y C H I K U N G U N Y A 
F C B U M T C M F Z N S I S A I D R A I G W T T  
;
RUN;

data Dictio;
	input search $ & 50.;
	TEMP=COMPRESS(SEARCH);
	TEMP_WORS=COUNTW(SEARCH);
CARDS;
WHOOPING COUGH
RABIES
MEASLES
INFLUENZA
FOOT
GIARDIASIS
EBOLA
CRYPTOSPORIDIOSIS
CHICKENPOX
AVIAN INFLUENZA
TYPHOID
PERTUSSIS
MALARIA
HIV
HAND
RUBELLA
DONOVANOSIS
CHOLERA
BRUCELLOSIS
ANTHRAM 
SYPHILIS
NOROVIRUS
LEPTOSPIROSIS
HEPATITIS
GLANDULAR FEVER
GASTROENTERITIS
DIPTHERIA
BOTULISM
SMALLPOX
MUMPS
LEPROSY
GONORRHOEA
FOODBORNE
DENGUE
CHIKUNGUNYA
BARMAH FOREST VIRUS
;
run;

***Creating format for restorying dictionary words**/;

data word_format;
 	 set dictio ;
 	 retain fmtname '$dict' type 'C';
 	 length label $50.;
	 rename temp = Start;
	 label = search;
RUN;

proc format cntlin=word_format;
run; 

****collecting dictionary values into dict macro variable*****;
proc sql noprint;
	select TEMP into:dict separated by " " from Dictio;
quit;

%put &dict.;


********Collecting total characters and numbers of columns into macro variables************;
DATA two;
	SET ONE;
	array ch _CHARACTER_;
	do over ch;
		k+1;
		end;
		
		call symputx("count",put(_n_,best.));
		call symputx("elem_count",k);
RUN;

%put &count. &elem_count;

***for each row how many combination of words(at least 2 characters) we can produce ,that count we storing in combination macro vriable**;
***example***
A B C D---------> AB ABC ABCD BC BCD CD
E F G H---------> EF EFG EFGH FG FGH GH
I J K L---------> IJ IJK IJKL JK JKL KL
M N O P---------> MN MNO MNOP NO NOP OP

4 LETTERS GIVE 6 POSSIBLITIES FOR EACH ROW 
***********;

data _null_;
	do i=int(&count.)-1 to 1 by -1;
		comb+i;
		end;
	call symputx("combination",comb);
run;

%put &combination.;


**HORIZONTAL AND VERTICAL SEARCH ARE PROGRAMMED TO SEARCH WITH SAME LOGIC**;
**JUST INTERCHANGING ALL ROWS WE CAN EXHIBIT VERTICAL SEARCH****************;

***********VERTICAL SEARCH***********************************************;
proc transpose data =one out=three;
	var _ALL_;
run;



/***
cycle-1 (j=1)
ch1=cats(ex[1],ex[2]);
ch2=cats(ch1,ex[3]);
ch3=cats(ch2,ex[4]);

cycle-2 (j=2)

ch4=cats(ex[2],ex[3]);
ch5=cats(ch4,ex[4]);

cycle-3 (j=3)
ch6=cats(ex[3],ex[4]);

*****/;

data vert_1;
	set three;
	length ch1-ch&combination. chr1-chr&combination.  $50.;
	array ex{*} $ col1-col&count.;
	array ch{&combination.} $ ;
	array chr{&combination.} $;

	p=0;
	j=1;
	doagain:
		do i=j to dim(ex)-1;
		p+1;
		if i=j then ch[p]=strip(cats(ex[i],ex[i+1])) ;
			else ch[p]=strip(cats(ch[p-1],ex[i+1]));
		chr[p]=strip(reverse(ch[p])); 
		end;
	if j<dim(ex)-1 then do;
		j+1;
		goto doagain;
	end;
run;

proc transpose data=vert_1 out=ver_search_down prefix=col;
	var ch1-ch&combination.;
run;

proc transpose data=vert_1 out=ver_search_reverse prefix=col;
	var chr1-chr&combination.;
run;


*******VERTICAL ENDS HERE******************;

*****HORIZONTAL SEARCH***********************;
data HORIZ_1;
	set ONE;
		length ch1-ch&combination. chr1-chr&combination.  $50.;
		array ex{*} $ _1-_24;

		array ch{&combination.} $ ;
		array chr{&combination.} $;

		p=0;
		j=1;
		doagain:
		do i=j to dim(ex)-1;
			p+1;
			if i=j then ch[p]=strip(cats(ex[i],ex[i+1])) ;
				else ch[p]=strip(cats(ch[p-1],ex[i+1]));
		chr[p]=strip(reverse(ch[p])); 
		end;
		
		if j<dim(ex)-1 then do;
				j+1;
				goto doagain;
				end;
run;


proc transpose data=HORIZ_1 out=hor_search_down prefix=col;
	var ch1-ch&combination.;
run;

proc transpose data=HORIZ_1 out=hor_search_reverse prefix=col;
	var chr1-chr&combination.;
run;



data ver_checking_down(keep=_name_ found_word found_column direction);
	set ver_search_down;
	length direction $25.;
	array ch{*} $ col1-col&count.; 
	do i=1 to &count.;
		if FINDW("&DICT",strip(ch[i]))>0 then do;
		found=1;
		found_word=put(ch[i],$dict.);
		found_column=vname(ch[i]);
		direction="look_vertical_down";
			end;
		end;
if found=1;
run;



data hor_checking(keep=_name_ found_word found_column direction);
		set hor_search_down;
		length direction $25.;
		array ch{*} $ col1-col&count.; 
			do i=1 to &count.;
				if FINDW("&DICT",strip(ch[i]))>0 then do;
				found=1;
				found_word=put(ch[i],$dict.);
				found_column=tranwrd(vname(ch[i]),"col","row");
				direction="look_horizontal_straight";
				end;
			end;
if found=1;
run;


data hor_checking_reverse(keep=_name_ found_word found_column direction);
		set hor_search_reverse;
		length direction $25.;
		array ch{*} $ col1-col&count.;
		do i=1 to &count.;
			if FINDW("&DICT",strip(ch[i]))>0 then do;
			found=1;
			found_word=put(ch[i],$dict.);
			found_column=tranwrd(vname(ch[i]),"col","row");
			direction="look_horizontal_reverse";
			end;
		end;
if found=1;
run;







data ver_checking_reverse(keep=_name_ found_word found_column direction);
		set ver_search_reverse;
		length direction $25.;
		array ch{*} $ col1-col&count.;
		do i=1 to &count.;
			if FINDW("&DICT",strip(ch[i]))>0 then do;
				found=1;
				found_word=put(ch[i],$dict.);
				found_column=vname(ch[i]);
				direction="look_vertical_reverse";
				end;
			end;
if found=1;
run;



data straights_found(DROP=_:);
	set ver_checking_down ver_checking_reverse  hor_checking_reverse hor_checking;
run;



****************************************
****right cross combinations****;
*****************************************



***CH ARRAY IS FOR OLD VARIABLES REFERRING ARRAY******;
DATA TOW_R_CROSS_1(KEEP=REQUIRED_VAR);
	SET ONE;
		ARRAY CH{*} $ _character_;
		ARRAY NEW_VAR{&elem_count.} $;
		DO I=1 TO &count.;
			T+1;
			IF T>&elem_count. THEN LEAVE; 
			NEW_VAR[T]=CH[I];
			REQUIRED_VAR=SUBSTR(REVERSE(CATS(OF NEW_VAR{*})),1,1);
			OUTPUT;
		END;
RUN;

PROC TRANSPOSE DATA=TOW_R_CROSS_1 OUT=TOW_R_CROSS_2(DROP=_NAME_);
	VAR REQUIRED_VAR;
RUN;

data overall_count;
	c=&combination.;
	do i=(&COUNT.-2) to 1 by -1;
		c+i;
	end;
	call symputx("overall_cross_comb_count",c);
run;




data TOW_R_CROSS_3(KEEP=COL: NE:);
		set TOW_R_CROSS_2;
		LENGTH NE1-NE&combination. $50. NE_1-NE_&combination. $50.;
		ARRAY EX{*} $ _CHARACTER_;
		ARRAY NE{&combination.} $;
		ARRAY NE_{&combination.} $;
		o=&count.;
		retain o;
		do j=1 to &count.-1;
			i=1;
			k=&count.*o;
			p=j+((&count.+1)*i);
			TEMP=1;
			LOOP_BACK:if p<=k then do;
			iteration+1;
			if TEMP=1 then do;
				ne[iteration]=cats(ex[j],ex[p]);
				ne_[iteration]=cats("ROW-1","COL-",put(j,best.));
				end;
			else do;
				ne[iteration]=cats(ne[iteration-1],ex[j+((&count.+1)*i)]);
				ne_[iteration]=cats("ROW-1","COL-",put(j,best.));
				end;
			TEMP+1;
			i+1;
			p=j+((&count.+1)*i);
			GO TO LOOP_BACK;
			end;
			o=o-1;
		end;
run;

%LET ITER2=%EVAL(&combination.+1);

%PUT &ITER2;


DATA TOW_R_CROSS_4(KEEP=COL: NE&ITER2.-NE&overall_cross_comb_count. NE_&ITER2.-NE_&overall_cross_comb_count.
drop=NE1-NE&combination.);
	SET TOW_R_CROSS_3(keep=col:);
	LENGTH NE&ITER2-NE&overall_cross_comb_count. $50.  NE_&ITER2.-NE_&overall_cross_comb_count. $50.;
	array ex{*} $ _CHARACTER_ ;
	array ne{&overall_cross_comb_count.} $;
	array ne_{&overall_cross_comb_count.} $;
	Row=2;
	j=&count.+1;
	ITERATION=&combination.+1;
BACK2:
	i=1;
	TEMP=1;
	do;
	p=j+((&count.+1)*i);
BACK:
	if TEMP=1 then do;
		ne[iteration]=cats(ex[j],ex[p]);
		ne_[iteration]=cats("ROW-",put(row,best.));
	end;
	else do;
		ne[iteration]=cats(ne[iteration-1],ex[p]);
		ne_[iteration]=cats("ROW-",put(row,best.));
	end;
	iteration+1;
	i+1;
	p=j+((&count.+1)*i);
	temp+1;
	if p<&elem_count. then do;goto BACK;end;
	end;
	if j<(&count.*(&count-2))+1 then do;
		j=j+(&count.);
		go to BACK2;
	end;
RUN;


DATA TOW_R_CROSS_5;
	MERGE  TOW_R_CROSS_3 TOW_R_CROSS_4;
	BY COL1-COL&COUNT.;
RUN;

%PUT &overall_cross_comb_count.;



proc transpose data=TOW_R_CROSS_5 out=TOW_R_CROSS_6_val;
	var ne1-ne&overall_cross_comb_count.;
run;

proc transpose data=TOW_R_CROSS_5 out=TOW_R_CROSS_6_pos(rename=(col1=cross_found_in));
	var ne_1-ne_&overall_cross_comb_count.;
run;

%let dict_count=%sysfunc(countw(&dict.," "));

%put &dict_count.;


data TOW_R_CROSS_6;
	merge TOW_R_CROSS_6_val TOW_R_CROSS_6_pos;
run;



DATA TOW_R_CROSS_7;
	SET TOW_R_CROSS_6;
	LENGTH DICT1-DICT&dict_count. $50. DIRECTION $50.;
	COL2=STRIP(REVERSE(COL1));
	ARRAY DICT{&dict_count.} $ ;
	DO I=1 TO DIM(DICT);
		DICT[I]=SCAN("&DICT",I," ");
		IF INDEX(STRIP(COL1),STRIP(DICT[I]))>0 THEN DO;
		FOUND_WORD=put(DICT[I],$dict.);
		FOUND=1;
		DIRECTION="LOOK_FROM_TOP_LEFT:CROSS_DIRECTION";
		found_column=strip(cross_found_in);
	END;

	IF INDEX(STRIP(COL2),STRIP(DICT[I]))>0 THEN DO;
		FOUND_WORD=put(DICT[I],$dict.);
		FOUND=1;
		DIRECTION="LOOK_FROM_BOTTOM_RIGHT:CROSS_DIRECTION";
		found_column=strip(cross_found_in);
		END;
	END;
IF FOUND=1;
RUN;

proc sort data=TOW_R_CROSS_7 OUT =TOWARDS_RIGHT_CROSS(keep=keep=FOUND_WORD DIRECTION found_column) NODUPKEY;
	BY FOUND_WORD;
RUN;

***************************************
*****LEFT  COMBINATIONS********;
****************************************;

DATA REVERSE_ONE;
	SET ONE;
		ARRAY ORIG{*} _CHARACTER_;
		ARRAY COL{*} $ COL1-COL&COUNT.;
		ORIG_DIM=DIM(ORIG)+1;
		DO I=1 TO DIM(COL);
		COL[I]=ORIG[ORIG_DIM-I];
		END;
	KEEP COL:;
RUN;

DATA TOW_L_CROSS_1(KEEP=REQUIRED_VAR);
		SET REVERSE_ONE;
		ARRAY CH{*} $ _character_;
		ARRAY NEW_VAR{&elem_count.} $;
		DO I=1 TO &count.;
			T+1;
			IF T>&elem_count. THEN LEAVE; 
			NEW_VAR[T]=CH[I];
			REQUIRED_VAR=SUBSTR(REVERSE(CATS(OF NEW_VAR{*})),1,1);
		OUTPUT;
		END;
RUN;



PROC TRANSPOSE DATA=TOW_L_CROSS_1 OUT=TOW_L_CROSS_2(DROP=_NAME_);
	VAR REQUIRED_VAR;
RUN;



data TOW_L_CROSS_3(KEEP=COL: NE:);
	set TOW_L_CROSS_2;
	LENGTH NE1-NE&combination. $50. NE_1-NE_&combination. $50.;
	ARRAY EX{*} $ _CHARACTER_;
	ARRAY NE{&combination.} $;
	ARRAY NE_{&combination.} $;
	o=&count.;
	retain o;
		do j=1 to &count.-1;
			i=1;
			k=&count.*o;
			p=j+((&count.+1)*i);
			TEMP=1;
LOOP_BACK:if p<=k then do;
			iteration+1;
			if TEMP=1 then do;
				ne[iteration]=cats(ex[j],ex[p]);
				ne_[iteration]=cats("ROW-1","COL-",put(j,best.));
				end;
			else do;
				ne[iteration]=cats(ne[iteration-1],ex[j+((&count.+1)*i)]);
				ne_[iteration]=cats("ROW-1","COL-",put(j,best.));
				end;
			TEMP+1;
			i+1;
			p=j+((&count.+1)*i);
			GO TO LOOP_BACK;
			end;
			o=o-1;
			end;
run;


DATA TOW_L_CROSS_4(KEEP=COL: NE&ITER2.-NE&overall_cross_comb_count.  NE_&ITER2.-NE_&overall_cross_comb_count.
	drop=NE1-NE&combination.);
		SET TOW_L_CROSS_3(keep=col:);
		LENGTH NE&ITER2.-NE&overall_cross_comb_count. $50. NE_&ITER2.-NE_&overall_cross_comb_count. $50.;
		array ex{*} $ _CHARACTER_ ;
		array ne{&overall_cross_comb_count.} $;
		array ne_{&overall_cross_comb_count.} $;
		Row=2;
		j=&count.+1;
		ITERATION=&combination.+1;
		BACK2:
		i=1;
		TEMP=1;
do;
		p=j+((&count.+1)*i);
BACK:
		if TEMP=1 then do;
			ne[iteration]=cats(ex[j],ex[p]);
			ne_[iteration]=cats("ROW-",put(row,best.));
			end;

		else do;
			ne[iteration]=cats(ne[iteration-1],ex[p]);
			ne_[iteration]=cats("ROW-",put(row,best.));
			end;
	iteration+1;
	i+1;
	p=j+((&count.+1)*i);
	temp+1;
	if p<&elem_count. then do;goto BACK;end;end;
	if j<(&count.*(&count-2))+1 then do;
	j=j+(&count.);
	go to BACK2;
	end;
RUN;

DATA TOW_L_CROSS_5;
	MERGE  TOW_L_CROSS_3 TOW_L_CROSS_4;
	BY COL1-COL&COUNT.;
RUN;



proc transpose data=TOW_L_CROSS_5 out=TOW_L_CROSS_6;
	var ne1-ne&overall_cross_comb_count.;
run;



proc transpose data=TOW_L_CROSS_5 out=TOW_L_CROSS_6_val;
	var ne1-ne&overall_cross_comb_count.;
run;

proc transpose data=TOW_L_CROSS_5 out=TOW_L_CROSS_6_pos(rename=(col1=cross_found_in));
	var ne_1-ne_&overall_cross_comb_count.;
run;

data TOW_L_CROSS_6;
	merge TOW_L_CROSS_6_val TOW_L_CROSS_6_pos;
run;


DATA TOW_L_CROSS_7;
	SET TOW_L_CROSS_6;
	LENGTH DICT1-DICT&dict_count. $50. DIRECTION $50.;
	COL2=STRIP(REVERSE(COL1));
	ARRAY DICT{&dict_count.} $ ;
	DO I=1 TO DIM(DICT);
		DICT[I]=SCAN("&DICT.",I," ");
		IF INDEX(STRIP(COL1),STRIP(DICT[I]))>0 THEN DO;
		FOUND_WORD=put(DICT[I],$dict.);
		FOUND=1;
		DIRECTION="LOOK_FROM_TOP_RIGHT:CROSS_DIRECTION";
		found_column=strip(cross_found_in);
		END;
	IF INDEX(STRIP(COL2),STRIP(DICT[I]))>0 THEN DO;
		FOUND_WORD=put(DICT[I],$dict.);
		FOUND=1;
		DIRECTION="LOOK_FROM_BOTTOM_LEFT:CROSS_DIRECTION";
		found_column=strip(cross_found_in);
		END;
		END;
IF FOUND=1;
RUN;

proc sort data=TOW_L_CROSS_7 OUT =TOWARDS_LEFT_CROSS(keep=FOUND_WORD DIRECTION found_column) NODUPKEY;
	BY FOUND_WORD;
RUN;

DATA CROSS_MATCHES;
	SET  TOWARDS_RIGHT_CROSS TOWARDS_LEFT_CROSS;
RUN;

******************************************************;

proc datasets lib=quiz1_a kill;
run;

proc datasets lib=work nolist;
	copy out=quiz1_a;
	select CROSS_MATCHES straights_found;
run;



proc datasets lib = work nodetails nolist nowarn;
    delete tow_: ver_: hor_:;
quit;


ods rtf file="/home/u50017288/sasuser.v94/practice/QUIZ_ANSWERS" style=ocean;
proc report data =straights_found;
	title "These are straight founds, either horizontal or vertical direction ";
run;

proc report data=cross_matches;
	title "These are diagonals founds ";
run;
ods rtf close;
