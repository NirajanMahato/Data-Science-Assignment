library(tidyverse)

pop_2011=read_csv("D:/Sem4/Data Science/Obtained Data/Population2011_1656567141570.csv")

pop_2020=pop_2011
pop_2020$Population=as.integer(pop_2020$Population*1.00561255390388033)


pop_2021=pop_2020
pop_2021$Population=as.integer(pop_2021$Population*1.00561255390388033)

pop_2022=pop_2021
pop_2022$Population=as.integer(pop_2022$Population*1.00561255390388033)

pop_2023=pop_2022
pop_2023$Population=as.integer(pop_2023$Population*1.00561255390388033)

sum(pop_2011$Population)
sum(pop_2020$Population)
sum(pop_2021$Population)
sum(pop_2022$Population)
sum(pop_2023$Population)


write_csv(pop_2020,"D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Population/pop2020.csv",append = FALSE)
write_csv(pop_2021,"D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Population/pop2021.csv",append = FALSE)
write_csv(pop_2022,"D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Population/pop2022.csv",append = FALSE)
write_csv(pop_2023,"D:/Sem4/Data Science/Data-Science-Assignment/Cleaned Data/Population/pop2023.csv",append = FALSE)


View(pop_2020)