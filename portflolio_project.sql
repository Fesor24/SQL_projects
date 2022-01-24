-- original work by Onafeso Olawale
-- creating a school database
-- analyzing its dataset to provide answers to relevant questions


-- Thank you

CREATE DATABASE IF NOT EXISTS school;

USE school;

CREATE TABLE students (
    stud_no INT NOT NULL,
    first_name VARCHAR(40) NOT NULL,
    last_name VARCHAR(40) NOT NULL,
    date_of_birth DATE NOT NULL,
    email_address VARCHAR(255) NOT NULL,
    phone_no INT(11) NOT NULL,
    course_rep INT,
    course_completed ENUM('Yes', 'No') NOT NULL,
    dormitory_id INT NOT NULL,
    PRIMARY KEY (stud_no),
    UNIQUE KEY (email_address , phone_no)
);

CREATE TABLE dormitory (
    id INT NOT NULL,
    dorm_name VARCHAR(40) NOT NULL,
    PRIMARY KEY (id),
    UNIQUE KEY (dorm_name)
);

CREATE TABLE lecturer (
    id INT NOT NULL,
    first_name VARCHAR(40) NOT NULL,
    last_name VARCHAR(40) NOT NULL,
    gender ENUM('M', 'F') NOT NULL,
    salary INT NOT NULL,
    PRIMARY KEY (id)
);

CREATE TABLE head_of_dept (
    hod_id INT NOT NULL,
    first_name VARCHAR(40) NOT NULL,
    last_name VARCHAR(40) NOT NULL,
    gender ENUM('M', 'F') NOT NULL,
    salary INT NOT NULL,
    PRIMARY KEY (hod_id)
);

CREATE TABLE department (
    id INT NOT NULL,
    dept_name VARCHAR(40) NOT NULL,
    head_of_dept_id INT NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (head_of_dept_id)
        REFERENCES head_of_dept (hod_id),
    UNIQUE KEY (dept_name)
);

CREATE TABLE course (
    course_code INT NOT NULL,
    course_name VARCHAR(40) NOT NULL,
    lecturer_id INT NOT NULL,
    department_id INT NOT NULL,
    PRIMARY KEY (course_code),
    FOREIGN KEY (lecturer_id)
        REFERENCES lecturer (id)
        ON DELETE CASCADE
);

CREATE TABLE students_majors (
    stud_id INT NOT NULL,
    course_code INT NOT NULL,
    FOREIGN KEY (course_code)
        REFERENCES course (course_code),
    FOREIGN KEY (stud_id)
        REFERENCES students (stud_no)
)
;

-- altering my students table to include the gender column after the last name column
ALTER TABLE students
ADD COLUMN gender ENUM('M','F') after last_name;

-- checking my students table to see if the column was rightly placed
SELECT 
    *
FROM
    students;

INSERT INTO students(stud_no,first_name,last_name,gender,date_of_birth,email_address,phone_no,course_completed,dormitory_id)
VALUES 
('1406','Wale','Onafeso','M','1998-04-24','w.onafeso@student.com','7004891','Yes','1004');

INSERT INTO students(stud_no,first_name,last_name,gender,date_of_birth,email_address,phone_no,course_rep,course_completed,dormitory_id)
VALUES 
('1666','Peter','Drury','M','1995-08-20','drury@student.com','0114891','1406','No','1014'),
('0023','Mary','Griffin','F','1999-09-01','m.griffin@student.com','0132190','1407','No','1014'),
('1253','Sandra','Peterson','F','1990-02-20','petersons@student.com','0117771','1406','No','1003'),
('1201','Mark','Spencer','M','2002-08-20','spencer@student.com','0114002','1407','Yes','1003'),
('0212','Charles','Drury','M','1994-08-20','c.drury@student.com','2014201','1407','Yes','1014'),
('1222','Nicki','Minaj','F','1993-12-02','minaj@student.com','0209891','1408','Yes','1016'),
('3006','Katy','Perry','F','1990-02-20','Perry@student.com','0224003','1407','Yes','1022'),
('2106','Mark','Henry','M','2001-04-30','henry@student.com','0113391','1408','No','1022'),
('1422','Jon','Snow','M','1995-08-20','snow@student.com','0114891','1407','No','1016'),
('0129','Kim','Carter','F','1989-12-02','carter@student.com','2014341','1406','Yes','1004'),
('1211','cersei','lannister','F','1995-08-20','lannister@student.com','0914021','1408','No','1014'),
('1748','Sheldon','Cooper','M','2000-08-30','sheldon@student.com','0933891','1407','No','1016'),
('1200','Lewis','Griffin','M','1999-08-20','lewis@student.com','0119961','1406','Yes','1022');

INSERT INTO students(stud_no,first_name,last_name,gender,date_of_birth,email_address,phone_no,course_completed,dormitory_id)
VALUES 
('1407','Timmy','Turner','M','1996-07-10','turner@student.com','2001291','No','1004'),
('1408','Rachael','Mikaelson','F','2000-11-20','rachael@student.com','0212916','Yes','1016');

INSERT INTO dormitory(id,dorm_name)
VALUES 
('1004','Iron Throne'),
('1014','Queens Quarters'),
('1003','White House'),
('1016','Aso Villa'),
('1022','Oval Office');

INSERT INTO lecturer(id,first_name,last_name,gender,salary)
VALUES
('21','Thomas','Shelby', 'M','250000'),
('22','Mike','Ross','M','300000'),
('23','Donna','Paulson','F','250000'),
('24','Janet', 'Washington','F','250000'),
('25','Laura','Shaw', 'F', '200000');

INSERT INTO head_of_dept(hod_id,first_name,last_name,gender,salary)
VALUES 
('001','Michael','Gray','M','100000'),
('002','Arthur','Shelby','M','200000'),
('003','Jamal','Lyon','M','90000'),
('004','Anjola','Jackson','F','85000'),
('005','Esther','Niles','F','300000');

INSERT INTO department(id,dept_name,head_of_dept_id)
VALUES 
('51','Law','001'),
('52','Science','002'),
('53','Social science','003'),
('54','Arts','004'),
('55','Business administration','005');

INSERT INTO course(course_code,course_name,lecturer_id,department_id)
VALUES
('101','Law','21','51'),
('102','Biology','22','52'),
('103','Psychology','23','53'),
('104','English','24','54'),
('105','Finance','25','55');

INSERT INTO students_majors(stud_id,course_code)
VALUES 
('1666','105'),
('0023','103'),
('1253','104'),
('1201','101'),
('0212','102'),
('1222','101'),
('3006','103'),
('2106','102'),
('1422','104'),
('0129','105'),
('1211','102'),
('1748','101'),
('1200','104'),
('1406','101'),
('1407','102'),
('1408','103');

-- extract the first name and last name of all students and their respective course representatives
-- where the student is a course representative, input 'is a course rep'
-- sort your data by the students first name

SELECT 
    v.first_name,
    v.last_name,
    COALESCE(v.course_rep_full_name,
            'is a course rep') AS course_representative
FROM
    (SELECT 
        s.first_name,
            s.last_name,
            CONCAT(cr.first_name, ' ', cr.last_name) AS course_rep_full_name
    FROM
        students s
    LEFT JOIN students cr ON s.course_rep = cr.stud_no) v
ORDER BY v.first_name;

-- what are the ages of the various students
-- create a new column 'scholarship'
-- if student is older than 25, input 'is not entitled to scholarship'
-- if student is 25 or younger than 25, input 'is entitled to scholarship'
-- sort by age

SELECT 
    st.first_name,
    st.last_name,
    st.age,
    CASE
        WHEN st.age > 25 THEN 'is not entitled to scholarship'
        ELSE 'is entitled to scholarship'
    END AS scholarship
FROM
    (SELECT 
        first_name,
            last_name,
            TIMESTAMPDIFF(YEAR, date_of_birth, CURRENT_DATE()) AS age
    FROM
        students) st
ORDER BY st.age;

-- count the number of students taking each course

SELECT 
    c.course_name, COUNT(sm.stud_id) AS total_num_of_students
FROM
    students_majors sm
        JOIN
    course c ON sm.course_code = c.course_code
GROUP BY c.course_name
ORDER BY total_num_of_students;

-- extract all records from the students table
-- exclude the records of students whose first name contains the letter 'M'

SELECT 
    *
FROM
    students
WHERE
    first_name NOT LIKE '%m%';
    
    -- compare the average salary of male and female lecturers
    -- round to 2 cents
   SELECT 
    gender, ROUND(AVG(salary), 2) AS avg_salary
FROM
    lecturer
GROUP BY gender;

-- which department head earns below 150000 and what department do they belong to

SELECT 
    hd.first_name, hd.last_name, d.dept_name, hd.salary
FROM
    head_of_dept hd
        JOIN
    department d ON d.head_of_dept_id = hd.hod_id
WHERE
    hd.salary < 150000;
    
    -- which dormitory has the highest number of student occupants
    
   SELECT 
    COUNT(s.stud_no) AS num_of_students_occupants, d.dorm_name
FROM
    students s
        JOIN
    dormitory d ON s.dormitory_id = d.id
GROUP BY d.dorm_name
ORDER BY num_of_students_occupants DESC;

-- create a procedure called 'course_info' that returns the course taken by a student
-- use student first name and last name as parameters

DELIMITER $$
CREATE PROCEDURE course_info(
	IN p_first_name VARCHAR(40), 
	IN p_last_name VARCHAR(40), 
	OUT p_course_name VARCHAR(40)
    )
BEGIN
	SELECT c.course_name
	INTO p_course_name
	FROM 
		students s 
			JOIN 
				students_majors sm ON s.stud_no=sm.stud_id
			JOIN 
				course c ON c.course_code=sm.course_code
	WHERE s.first_name=p_first_name
	AND s.last_name=p_last_name;
END $$
DELIMITER ;

-- find the percentage of students that have completed their respective course 
-- also find the percentage of students who have not
SELECT 
    m.num_of_students,
    m.num_of_students/16 *100 AS percentage_num_of_students,
    m.course_completed
FROM
    (SELECT 
        COUNT(distinct stud_no) AS num_of_students, course_completed
    FROM
        students
    GROUP BY course_completed) m
GROUP BY m.course_completed;

-- in the statement above, 16 is the total number of students in the students table
-- to arrive at 16, I used the COUNT FUNCTION to count the total number of students
-- included the distinct function to avoid duplicates

SELECT 
    COUNT(DISTINCT stud_no) AS num_of_students
FROM
    students;
    
-- how many students are born in the month of August

SELECT 
    COUNT(n.stud_no) AS num_of_students, n.month
FROM
    (SELECT 
        stud_no, MONTHNAME(date_of_birth) AS month
    FROM
        students) n
WHERE
    n.month = 'August';
    
-- retrieve all records of all female students
-- sort by first name and then last name

SELECT 
    *
FROM
    students
WHERE
    gender = 'F'
ORDER BY first_name , last_name;


-- That will be all for this project
-- Thank you











