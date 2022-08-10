/*drop table PULP_INDEX;*/
create table PULP_INDEX (PLT_CN varchar2(15), PULP_INDEX NUMBER(12,4));
grant select on PULP_INDEX to bbutler01;
select * from PULP_INDEX;
