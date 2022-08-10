/*drop table MILL_INDEX;*/
create table MILL_INDEX (PLT_CN varchar2(15), PULP_INDEX NUMBER(12,4), PULP_INDEX_100 NUMBER(12,4), SAW_INDEX NUMBER(12,4), SAW_INDEX_500 NUMBER(12,4));
grant select on MILL_INDEX to bbutler01;
select * from MILL_INDEX;
