# Assumption: Data cleaning has been completed (e.g. falied attention checks have been removed)
# Data Structure
# enemy | binary | Exposure to enemy image treatment
# pre.reason | float [0,1] | Computed pre-reasoning value
# post.reason | float [0,1] | Computed post-reasoning value
# pre.confidence | integer [1,10] | Reported confidence level prior to new information
# post.confidence | integer [1,10] | Reported confidence level after new information
# cyber.event | float [0,1] | Computed knowledge of cybersecurity incidents
# cyber.crime | float [0,1] | Computed experience of cyber crime
# risk | float [0,1] | Computed risk tolerance
# age | integer | Reported participant age
# male | integer [0,1] | Reported participant gender
# country | string | Reported participant nationality

library(Matching)
library(mediation)

# Task 1 Data Pre-Processing
# Group into Five Eyes
data$five.eyes <- ifelse(data$country == 'United States' | data$country == 'United Kingdom' | data$country == 'Canada' | data$country == 'Australia' | data$country == 'New Zealand', 1, 0)
# Compute confidence shift
data$shift.conf <- data$post.conf - data$pre.conf
# Task 2 Data summarization
summary(data$age)
sd(data$age)
table(data$male)
table(data$country)
summary(data$risk)
sd(data$risk)
summary(data$cyber.crime)
sd(data$cyber.crime)
summary(data$cyber.event)
sd(data$cyber.event)
summary(data$pre.conf)
sd(data$pre.conf)
summary(data$shift.conf)
sd(data$shift.conf)
summary(data$pre.reason)
sd(data$pre.reason)
summary(data$post.reason)
sd(data$post.reason)

# Task 3 Co-Variance Balance
# Numeric co-variants
MatchBalance(enemy~cyber.event+cyber.crime+risk+age+male+five.eyes, data=data, nboots=20)

# Task 4 Outcome versus Treatment Comparisons
t.test(data$pre.conf[data$enemy==0], data$pre.conf[data$enemy==1])
t.test(data$shift.conf[data$enemy==0], data$shift.conf[data$enemy==1])
t.test(data$pre.reason[data$enemy==0], data$pre.reason[data$enemy==1])
t.test(data$post.reason[data$enemy==0], data$post.reason[data$enemy==1])

# Task 4 Causal Analysis
# Total Effects Models - Confidence
summary(lm(pre.conf ~ enemy, data = data))
summary(lm(pre.conf ~ enemy + cyber.event + cyber.crime + risk, data = data))
summary(lm(pre.conf ~ enemy + cyber.event + cyber.crime + risk + age + male + five.eyes, data = data))
# Total Effects Models - Confidence Shift
summary(lm(shift.conf ~ enemy, data = data))
summary(lm(shift.conf ~ enemy + cyber.event + cyber.crime + risk, data = data))
summary(lm(shift.conf ~ enemy + cyber.event + cyber.crime + risk + age + male + five.eyes, data = data))
# Average Direct Effects Models - Confidence
summary(lm(pre.conf ~ enemy + pre.reason, data = data))
summary(lm(pre.conf ~ enemy + pre.reason + cyber.event + cyber.crime + risk, data = data))
summary(lm(pre.conf ~ enemy + pre.reason + cyber.event + cyber.crime + risk + age + male + five.eyes, data = data))
# Average Direct Models - Confidence Shift
summary(lm(shift.conf ~ enemy + post.reason, data = data))
summary(lm(shift.conf ~ enemy + post.reason + cyber.event + cyber.crime + risk, data = data))
summary(lm(shift.conf ~ enemy + post.reason + cyber.event + cyber.crime + risk + age + male + five.eyes, data = data))
# Mediator Models - Pre-Reasoning
summary(lm(pre.reason ~ enemy, data = data))
summary(lm(pre.reason ~ enemy + cyber.event + cyber.crime + risk, data = data))
summary(lm(pre.reason ~ enemy + cyber.event + cyber.crime + risk + age + male + five.eyes, data = data))
# Mediator Models - Post-Reasoning
summary(lm(post.reason ~ enemy, data = data))
summary(lm(post.reason ~ enemy + cyber.event + cyber.crime + risk, data = data))
summary(lm(post.reason ~ enemy + cyber.event + cyber.crime + risk + age + male + five.eyes, data = data))

# Mediation Analysis
# Enemy > Pre-Reasoning > Confidence
mod.conf <- lm(pre.conf ~ enemy + pre.reason, data = data)
mod.pre.reason <- lm(pre.reason ~ enemy, data = data)
result.pre.conf <- mediate(mod.pre.reason, mod.conf, treat='enemy', mediator='pre.reason', boot=TRUE, sims=500)
summary(result.pre.conf)
# Enemy > Post-Reasoning > Confidence Shift
mod.shift.conf <- lm(shift.conf ~ enemy + post.reason, data = data)
mod.post.reason <- lm(post.reason ~ enemy, data = data)
result.post.conf <- mediate(mod.post.reason, mod.shift.conf, treat='enemy', mediator='post.reason', boot=TRUE, sims=500)
summary(result.post.conf)
# Cyber Event Awareness > Post-Reasoning > Confidence Shift
mod.shift.conf <- lm(shift.conf ~ cyber.event + post.reason, data = data)
mod.post.reason <- lm(post.reason ~ cyber.event, data = data)
result.post.conf <- mediate(mod.post.reason, mod.shift.conf, treat='cyber.event', mediator='post.reason', boot=TRUE, sims=500)
summary(result.post.conf)
