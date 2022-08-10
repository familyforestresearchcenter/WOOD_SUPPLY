/*drop table LAND_COVER;*/
create table LAND_COVER (PLT_CN varchar2(15), AG_PROP NUMBER(7,5), URBAN_PROP NUMBER(7, 5), FOREST_PROP NUMBER(7, 5));
grant select on LAND_COVER to bbutler01;
select * from LAND_COVER;
