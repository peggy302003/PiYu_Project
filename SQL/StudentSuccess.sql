# Grades vs activities
select math_grade,por_grade
from (select
avg(g3) as math_grade
from studentmat) as t1,
(
select
avg(g3) as por_grade
from studentpor) as t2
;

# higher educations vs activities
select math_higher,por_higher
from (select
avg(case when higher='yes' then 1 else 0 end) as math_higher
from studentmat) as t1,
(
select
avg(case when higher='yes' then 1 else 0 end) as por_higher
from studentpor) as t2
;
# romantic vs higher and absense 
select t1.romantic, math_grade,math_grade,math_higher,por_grade,por_grade,por_higher
from (select
romantic,
avg(g3) as math_grade,
avg(absences) as math_absences,
avg(case when higher='yes' then 1 else 0 end) as math_higher
from studentmat
group by romantic) as t1
left join (
select
romantic,
avg(g3) as por_grade,
avg(absences) as por_absences,
avg(case when higher='yes' then 1 else 0 end) as por_higher
from studentpor
group by romantic) as t2 on t1.romantic=t2.romantic
;

## good students label 
select t1.promising_student,math_score,math_higher,math_count,por_score,por_higher,por_count
from (select 
case when romantic='No'and activities='Yes' and dalc in (1,2) and walc in (1,2) and medu in (3,4) and fedu in (3,4)
then 'Good' else 'Average' end as promising_student,
avg(g3) as math_score,
avg(case when higher='yes' then 1 else 0 end) as math_higher,
count(*) as math_count
from studentmat
group by promising_student
) as t1
left join
(select 
case when romantic='No' and activities='Yes'  and dalc in (1,2) and walc in (1,2) and medu in (3,4) and fedu in (3,4)
then 'Good' else 'Average' end as promising_student,
avg(g3) as por_score,
avg(case when higher='yes' then 1 else 0 end) as por_higher,
count(*) as por_count
from studentpor
group by promising_student) as t2 on t1.promising_student=t2.promising_student
;

select 
avg(g3) as score
from studentmat
;
select 
activities,romantic,count(*)
from studentpor
group by activities,romantic
;


