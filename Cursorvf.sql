SET NOCOUNT ON;
declare @territory as int
DECLARE @tablevar AS table
(bonus money, salesquota money)

;
DECLARE cust_cursor CURSOR FAST_FORWARD FOR
  SELECT TerritoryID
  FROM Sales.SalesTerritory;

OPEN cust_cursor;
FETCH NEXT FROM cust_cursor INTO @territory;

WHILE @@FETCH_STATUS = 0
BEGIN
insert into @tablevar (bonus, salesquota)
  SELECT bonus, SalesQuota
  FROM sales.salesperson
WHERE territoryid = @territory; 



FETCH NEXT FROM cust_cursor INTO @territory;


END
select * from @tablevar;
CLOSE cust_cursor;
DEALLOCATE cust_cursor;
GO


