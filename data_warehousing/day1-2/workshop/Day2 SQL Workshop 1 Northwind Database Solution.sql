-- Note: In some question, there may be more than 1 approach!


-- Workshop on SQL Query
-- 1 a)
select * from Shippers;

-- 1 b)
select * from Shippers
order By CompanyName;

-- 2 a)
select FirstName,LastName,Title,BirthDate,City
from Employees;

-- 2 b)
select Distinct Title
from Employees;


-- 3)
select * from Customers
where City='London'
or City='Madrid';

-- 4)
select CustomerID,ContactName
from Customers
where Country='UK'
order By ContactName;

-- 5)
-- Using subquery
select OrderID,OrderDate
from Orders where CustomerId in 
	(select Customerid 
	 from Customers
     where Companyname ='Maison Dewey');

-- Using join
select OrderID,OrderDate
from Orders,Customers
where Orders.CustomerID=Customers.CustomerID
and Customers.CompanyName='Maison Dewey';


-- 6)

select C.CompanyName,COUNT(O.CustomerID) as 'Number Of Orders'
 from Customers C, Orders O
 Where C.CustomerID=O.CustomerID
 Group By C.CompanyName;



-- 7).a)
select count(*), O.CustomerID,C.CompanyName
from Orders O INNER JOIN Customers C
ON C.CustomerID=O.CustomerID
group by O.CustomerID, c.companyname
having count(*) > 10 order by count(*);

-- 7).b)
select count(*), O.CustomerID,C.CompanyName from Orders O INNER JOIN Customers C
ON C.CustomerID=O.CustomerID and c.customerid ='BONAP'
group by O.CustomerID, c.companyname

-- 7).c)
select count(*), O.CustomerID,C.CompanyName
from Orders O INNER JOIN Customers C
ON C.CustomerID=O.CustomerID
group by O.CustomerID, c.companyname
having count(*) > (select count(*) from Orders O INNER JOIN Customers C
ON C.CustomerID=O.CustomerID and c.customerid ='BONAP'
group by O.CustomerID)


-- 8)
select p.productname,sum(od.discount * od.unitprice * od.quantity) 
from [order details] od, products p
where p.productid = od.productid 
and p.productid = od.productid
group by p.productid, p.productname
order by sum (od.unitprice * od.quantity * od.discount) desc


-- 9)
select C.ContactName from Customers C
where C.City not in (select Distinct City from Suppliers);



-- 10)

select orderid,sum(quantity*unitprice) from [order details]
group by orderid
having 
sum(quantity*unitprice)>10000;



