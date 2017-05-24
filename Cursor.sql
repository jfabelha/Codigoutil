SET NOCOUNT ON;
DECLARE @territory AS INT;
DECLARE cust_cursor CURSOR FAST_FORWARD FOR
  SELECT TerritoryID
  FROM Sales.SalesTerritory;

OPEN cust_cursor;
FETCH NEXT FROM cust_cursor INTO @territory;

WHILE @@FETCH_STATUS = 0
BEGIN
  SELECT *
  FROM sales.salesperson
WHERE territoryid = @territory; 


FETCH NEXT FROM cust_cursor INTO @territory;
END;
CLOSE cust_cursor;
DEALLOCATE cust_cursor;
GO


