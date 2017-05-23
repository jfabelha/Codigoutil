CREATE FUNCTION function_name (@territory Int)
  RETURNS @terr Table 
    (terrt int null) 
AS 
  BEGIN 
    IF @territory IS NULL 
      BEGIN 
        INSERT INTO @terr(terrt) 
        SELECT TerritoryID 
        FROM sales.salesperson 
      END 
    ELSE 
      BEGIN 
        INSERT INTO @terr(terrt) 
        SELECT bonus
        FROM sales.salesperson 
        WHERE territoryid = @territory 
      END 
    RETURN 
  END 
GO

CREATE PROCEDURE terr 
  @territory int = NULL
AS 
SELECT *
  FROM sales.salesperson
WHERE territoryid in (select terrt from dbo.function_name(@territory))
GO 

EXEC terr
GO


drop procedure terr
drop function function_name