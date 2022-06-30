"# IBSCMasterFeedbackDash" 

This Dashboard is designed to give the skeleton framework for individual school feedback dashboards for the International Boys School Coalition Phase 2 Project.

To install any missing packages, uncomment and run the first part of packageSetup.R.

masterDash.RMD requires the requisite .RDS files from the working directory.  To generate these, download the .xlsx data from the requisite Polinode surveys to the working directory and run SchoolDataPrep.R.  Then run dashMaster.RMD and quality review.  Finally publish to shinyApps.io, adding each .RDS file and those files in the header,, either by adding them to masterDash.RMD YAML header, or via gui to shinyApps.io.

For reference, these are the questions asked of staff and students:
Staff Questions

*Q1 Age (21-30, 31-40, 41-50, 51-60 61+)
*Q2 Tenure (0-2 Years, 3-5 Years, 6-10 Years, 11-15 Years, 16+ Years)
*Q3 Gender (Male, Female, Non-Binary, Prefer not to say)
*Q4 Subject (Natural Sciences, Social Sciences, Health and Physical Education, The Arts (creative and performing), English, Mathematics, Languages, Other Humanities, Pastoral)
*Q5 Streamed by ability (Yes, No)
*Q6 Teach Previous Year? (Yes, No)
*Q7 Hours taught per week (less than 1, 1-2, 3-4, more than 4)
*Q8 How important are each of the following technologies?
*Q9 Relationship 1 – Communication Highly Effective (Strongly Disagree to Strongly Agree 6 point scale)
*Q10 Relationship 2 – Story or timeline
*Q11 Relationship 3 – Know this Student Well
*Q12 Relationship 4 – Relationship Fair and Respectful
*Q13 Relationship 5 – Alignment of Purpose and Values
*Q14 Relationship 6 – Extra Curricular contact opportunities present
*Q15 Tech helpful for developing and maintaining relationships?
*Q16 Tech unhelpful for developing and maintaining relationships?
*Q17 Greatest benefit of technology?
*Q18 Greatest impediment of technology?

Student Questions
*Q1 School Belongingness
1.2 I feel like an outsider (or left out of things) at school			
1.5 I make friends easily at school			
1.3 I feel like I belong at school		
1.1 I feel awkward and out of place in my school		
1.6 Other students seem to like me		
1.4 I feel lonely at school
*Q2 Relationship 1 – Frequent positive interactions (Strongly Disagree – Strongly Agree 6 point scale)
*Q3 Relationship 2 – Know one another’s skills and interests
*Q4 Relationship 3 – They value and respect my contributions
*Q5 Student-Teacher questions 
5.1 I can talk to or contact my teacher when I need to			
5.4 My teacher cares about me					
5.10 Neither my nor my teacher's reputation have made the relationship difficult		
5.2 It is worth building a good relationship with my teacher because I may be in a class or activity with them in the future
5.9 My teacher understands any particular needs or pressures I face
5.5 My teacher has a good understanding of my skills and interests
5.8 My teacher treats me fairly
5.7 My teacher recognizes and rewards my efforts
5.6 My teacher inspires and motivates me
5.3 My teacher and I have shared goals for my progress and development