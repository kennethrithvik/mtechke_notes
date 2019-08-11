-- Workshop on SQL DDL


1)
CREATE TABLE MemberCategories
(MemberCategory nVarChar(2) NOT NULL,
 MemberCatDescription nVarChar(200) NOT NULL,
PRIMARY KEY (MemberCategory)
)

2)
INSERT INTO MemberCategories(MemberCategory,MemberCatDescription) VALUES ('A','Class A Members');
INSERT INTO MemberCategories(MemberCategory,MemberCatDescription) VALUES ('B','Class B Members');
INSERT INTO MemberCategories(MemberCategory,MemberCatDescription) VALUES ('C','Class C Members');

3)CREATE TABLE GoodCustomers
(CustomerName nVarChar(50), 
Address nVarChar(65),
PhoneNumber nVarChar(9),
MemberCategory nVarChar(2),
PRIMARY KEY(CustomerName,PhoneNumber),
FOREIGN KEY(MemberCategory) REFERENCES MemberCategories (MemberCategory)
)

4)  ** Ignore this question

INSERT INTO GoodCustomers(CustomerName,PhoneNumber,MemberCategory)
SELECT CustomerName,PhoneNumber,MemberCategory FROM Customers
WHERE MemberCategory in ('A','B')


5)INSERT INTO GoodCustomers(CustomerName,PhoneNumber,MemberCategory)
VALUES ('TracyTan','736572','B')

6)
INSERT INTO GoodCustomers VALUES('Grace Leong','15 Bukit Purmei Road,Singapore 0904','278865','A')


7)INSERT INTO GoodCustomers VALUES('Lynn Lim','15 Bukit Purmei Road,Singapore 0904','278865','P')
-- violation of referential integrity

8)UPDATE GoodCustomers SET Address='22 Bukit Purmei Road,Singapore 0904'
WHERE CustomerName ='Grace Leong'


9)DELETE FROM GoodCustomers
WHERE CustomerName='Grace Leong'

   
10) CREATE INDEX CustomerNumber_idx ON GoodCustomers (CustomerName)

11) DELETE FROM GoodCustomers

12) DROP TABLE GoodCustomers