show tables;
--
 quit
--
spi2_b1_m1
--
select count(1) from address;
--
quit
--
select * from tabs;
--
describe tabs;
--
spool "D:\describe.out.txt"
--
describe tabs;
--
spool off
--
select table_name from tabs;
--
select table_name from tabs where table_name not like '%$%';
--
select table_name from tabs where table_name like '%SCHEMA%';
--
quit
--
