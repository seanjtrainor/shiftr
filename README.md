# shiftr
The MLB is banning the shift starting in 2023. I wanted to see how this would impact batting average on balls hit in play (BABIP) among batted balls 
that could be fielded by an infielder. I defined that as any ground ball, or any line drive with a hit distance of less than 224 feet. The data was 
pulled using a modified form of Bill Petti's "scrape_statcast_savant()" function from his baseballr package. I used pitch by pitch data from the 2019-
2021 seasons. My outcome variable in this analysis was whether or not the batted ball resulted in a hit. The inputs I used were if there was shift in 
place, exit velocity (launch_speed), where the ball was hit (angle), if the batter was lefty or righty (stand), launch_angle, and sprint speed of the 
batter. I originally used the 2019 and 2020 data as the training data and tested multiple classification machine learning models including logistic
regression, random forest, SVM, and BART. It turned out the random forest was the most accurate, so I fine tuned that. I ran the 2021 data untouched
through the model, and then the same data except I made shift = 0. As expected BABIP was slightly higher for every team when there was no shift in place.
