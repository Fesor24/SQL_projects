-- data was collected from articles and from the press
-- data was cleaned with python and imported to sql for analysis

-- we'll anlayze Nigeria's budget allocation to the education sector
-- UNESCO recommends to all developing countries a 15% budget allocation to education from total budget
-- we'll see how Nigeria's education budget allocation differs to the recommendation of UNESCO

-- select our database
USE education_budget;

-- lets view our table
SELECT 
    *
FROM
    education_budget_nigeria;
    
-- number of distinct records in the year column in our table
SELECT 
    COUNT(DISTINCT year) AS num_of_unique_years
FROM
    education_budget_nigeria;

-- percent of education budget by year
SELECT 
    year,
    total_budget,
    education_budget,
    (education_budget / total_budget) * 100 AS percent_education_budget
FROM
    education_budget_nigeria
ORDER BY year;

-- years where education budget is above 150 billion
SELECT 
    year, 
    education_budget
FROM
    education_budget_nigeria
WHERE
    education_budget >= 150000000000
ORDER BY year;

-- total amount spent on education from 2005 to 2022
SELECT 
    SUM(education_budget) AS total_amt_spent_on_edu
FROM
    education_budget_nigeria
WHERE
    year BETWEEN 2005 AND 2022;

-- UNESCO recommends developing countries give 15% of budget to education
-- using this standard, lets see the amount the education sector is meant to receive from the total budget
SELECT 
    year,
    education_budget,
    total_budget,
    ROUND((15 / 100) * total_budget, 2) AS UNESCO_recommendation
FROM
    education_budget_nigeria
ORDER BY year;

-- lets compare the difference between UNESCO education budget recommendation and Nigeria's education budget allocation
SELECT 
    year,
    n.total_budget,
    n.education_budget,
    n.UNESCO_recommendation,
    n.UNESCO_recommendation - n.education_budget AS diff_edu_budget_from_UNESCO
FROM
    (SELECT 
        year,
            education_budget,
            total_budget,
            ROUND((15 / 100) * total_budget, 2) AS UNESCO_recommendation
    FROM
        education_budget_nigeria) n
ORDER BY year;

-- which year had the highest budget education allocation
SELECT 
    n.year,
    n.education_budget,
    n.total_budget,
    MAX(n.percent_alloc_from_total_budget) AS highest_edu_budget_allocation
FROM
    (SELECT 
        year,
            education_budget,
            total_budget,
            (education_budget / total_budget) * 100 AS percent_alloc_from_total_budget
    FROM
        education_budget_nigeria) n;
    
-- which year had the lowest budget allocation to education
SELECT 
    n.year,
    n.education_budget,
    n.total_budget,
    MIN(n.percent_alloc_from_total_budget) AS lowest_edu_budget_allocation
FROM
    (SELECT 
        year,
            education_budget,
            total_budget,
            (education_budget / total_budget) * 100 AS percent_alloc_from_total_budget
    FROM
        education_budget_nigeria) n;
    
-- percent total education budget from 2010 to 2022
SELECT 
    SUM(education_budget) AS total_edu_budget,
    SUM(total_budget) AS total_nig_budget,
    (SUM(education_budget) / SUM(total_budget)) * 100 AS percent_alloc_education
FROM
    education_budget_nigeria
WHERE
    year >= 2010;
    
-- compare the education budget of each year with the previous year
SELECT 
	year, 
	education_budget, 
	LAG(education_budget) OVER() as last_edu_budget,
	education_budget - LAG(education_budget) OVER() as difference_from_last_edu_budget
FROM education_budget_nigeria
ORDER BY year;

-- create a stored procedure that returns the education budget allocation when the year is input
DELIMITER $$
CREATE PROCEDURE edu_budget_allocation(in p_year VARCHAR(25), out p_edu_allocation bigint)
BEGIN
	SELECT education_budget
    INTO p_edu_allocation
    FROM education_budget_nigeria
    WHERE year=p_year;
END $$
DELIMITER ;


