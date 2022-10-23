seasonnum1 <- 2018
seasonnum2 <- 2022
weeknum1 <- 1
weeknum2 <- 5
#总得分####
ccc<-load_schedules(2018:2022)%>%
  filter(week>=weeknum1, week<=weeknum2) %>%
  mutate(home_w=if_else(home_score>away_score,1,0),
         away_w=if_else(home_score<away_score,1,0),
         tie=if_else(home_score==away_score,1,0)
  )%>%
  collect()

total_pts<-ccc%>%
  group_by(season)%>%
  summarise(pts=sum(total),ptdf=sum(abs(away_score-home_score)),
            ptdf_h=sum(home_score-away_score),ptdf_a=sum(away_score-home_score),
            homewin=sum(home_w),awaywin=sum(away_w),tie=sum(tie),
            games=n()
  )%>%
  mutate(game_pts=pts/games,homewp=homewin/games)


#一共几次Touchdowns####
touchdowns<-pbp%>%
  filter(season>=seasonnum1, season<=seasonnum2, week>=weeknum1, week<=weeknum2, 
         !is.na(touchdown)) %>%
  select(game_id,defteam,qtr,desc,play_id, season, week, season_type, time, 
         posteam, td_team,  play_type, play_type_nfl,touchdown)%>%
  collect()

touchdowns02<-touchdowns%>%
  group_by(season)%>%
  summarise(TD=sum(touchdown))

#一共几次punt attempt####
punts<-pbp%>%
  filter(season>=seasonnum1, season<=seasonnum2, week>=weeknum1, week<=weeknum2,
         !is.na(punt_attempt)) %>%
  select(game_id,defteam,qtr,desc,play_id, season, week, season_type, time, 
         posteam, td_team,  play_type, play_type_nfl,punt_attempt)%>%
  collect()

punts02<-punts%>%
  group_by(season)%>%
  summarise(PUNTS=sum(punt_attempt))

punts03<-punts%>%
  group_by(season,team=posteam)%>%
  summarise(PUNTS=sum(punt_attempt))%>%
  drop_na()

#一共几次field goal attempt####
field_goals<-pbp%>%
  filter(season>=seasonnum1, season<=seasonnum2, week>=weeknum1, week<=weeknum2,
         !is.na(field_goal_attempt)) %>%
  select(game_id,defteam,qtr,desc,play_id, season, week, season_type, time, 
         posteam, td_team,  play_type, play_type_nfl,field_goal_attempt)%>%
  collect()

field_goals02<-field_goals%>%
  group_by(season)%>%
  summarise(FGS=sum(field_goal_attempt))

field_goals03<-field_goals%>%
  group_by(season,team=posteam)%>%
  summarise(FGS=sum(field_goal_attempt))%>%
  drop_na()

#一共几次fumble lost和int####
turnovers<-pbp%>%
  filter(season>=seasonnum1, season<=seasonnum2, week>=weeknum1, week<=weeknum2,
         !is.na(fumble_lost),!is.na(interception)) %>%
  select(game_id,defteam,qtr,desc,play_id, season, week, season_type, time, 
         posteam, td_team,  play_type, play_type_nfl,fumble_lost,interception)%>%
  collect()
turnovers02<-turnovers%>%
  group_by(season)%>%
  summarise(TOS=sum(fumble_lost)+sum(interception))

turnovers03<-turnovers%>%
  group_by(season,team=posteam)%>%
  summarise(TOS=sum(fumble_lost)+sum(interception))%>%
  drop_na()
#组合数据库#####
db_list <- list(total_pts,touchdowns02,field_goals02,punts02,turnovers02)
total_season<-db_list %>% reduce(full_join, by="season")

#总的drive数量#####
pbp_rp<-pbp%>%
  filter(season>=seasonnum1, season<=seasonnum2, week>=weeknum1, week<=weeknum2, 
         !is.na(posteam), !is.na(yardline_100))%>%
  filter(desc!="END QUARTER 1",desc!="END QUARTER 2",
         desc!="END QUARTER 3",desc!="END QUARTER 4",desc!="END GAME")%>%
  filter(drive_play_count!=0)%>%
  filter(play_type!="extra_point",play_type!="no_play",two_point_attempt!=1) %>% 
  select(play_id,game_id,season,week,posteam,down,desc,fixed_drive,
         fixed_drive_result,drive_inside20)%>%
  group_by(season,game_id,posteam,week,fixed_drive,fixed_drive_result)%>%
  summarize(redzone=mean(drive_inside20))%>%
  mutate(redzonetd=if_else(redzone==1&fixed_drive_result=="Touchdown",1,0))%>%
  mutate(redzonetov=if_else(redzone==1&fixed_drive_result=="Turnover",1,0))%>%
  mutate(redzoneopptd=if_else(redzone==1&fixed_drive_result=="Opp touchdown",1,0))%>%
  collect()

pbp_rp<-pbp_rp%>%
  mutate(redzonetov=if_else(redzone==1&redzoneopptd==1,1,redzonetov))
pbp_rp_df<-as.data.frame(pbp_rp)
drive_list<-pbp_rp_df%>%
  group_by(season,team=posteam)%>%
  summarise(off_drives=n(),redzone=sum(redzone),redzonetd=sum(redzonetd),
            redzonetov=sum(redzonetov),redzoneopptd=sum(redzoneopptd))

pbp_rp%>%
  group_by(fixed_drive_result)%>%
  tally()

#进攻组TD#####
OFF_TD<-pbp%>%
  filter(season>=seasonnum1, season<=seasonnum2, week>=weeknum1, week<=weeknum2, 
         touchdown==1, posteam==td_team,play_type!="kickoff") %>%
  select(game_id,defteam,qtr,desc,play_id, season, week, season_type, time, 
         posteam, td_team, play_type, play_type_nfl,touchdown)%>%
  collect()
OFF_TD03<-OFF_TD%>%
  group_by(season,team=td_team)%>%
  summarise(OFF_TDS=sum(touchdown))

#组合数据库#####
db_list <- list(drive_list,OFF_TD03,field_goals03,punts03,turnovers03)
total_season_off<-db_list %>% 
  reduce(full_join, by=c("season","team"))%>%
  mutate(Score_p=(OFF_TDS+FGS)/off_drives,td_p=OFF_TDS/off_drives,fg_p=FGS/off_drives,
         punt_p=PUNTS/off_drives,to_p=TOS/off_drives,red_p=redzone/off_drives,
         redtd_p=redzonetd/redzone,outredtd=OFF_TDS-redzonetd,
         outredtd_p=outredtd/off_drives)

#单节被零封####
C<-pbp%>%
  filter(season>=seasonnum1, season<=seasonnum2, week>=weeknum1, week<=weeknum2)%>%
  filter(play_type_nfl%in%c("GAME_START","END_QUARTER","END_GAME"))%>%
  select(season,week,game_id, away_team, home_team, qtr, total_away_score,total_home_score,down, play_id,  desc, score_differential_post,fixed_drive,play_type, play_type_nfl,play_type,)%>%
  collect()

C1<-C%>%
  group_by(season,week,game_id)%>%
  mutate(home_qtr_pts=total_home_score-lag(total_home_score),away_qtr_pts=total_away_score-lag(total_away_score))%>%
  ungroup()

C1<-C1%>%
  filter(!is.na(home_qtr_pts),qtr<=4)%>%
  mutate(total_qtr_pts=home_qtr_pts+away_qtr_pts)

home_qtr<-C1%>%
  select(season,week,game_id,team=home_team,qtr,qtr_pts=home_qtr_pts,total_qtr_pts)

away_qtr<-C1%>%
  select(season,week,game_id,team=away_team,qtr,qtr_pts=away_qtr_pts,total_qtr_pts)

qtr_pts<-rbind(home_qtr,away_qtr)%>%
  mutate(shutout=if_else(qtr_pts==0,1,0))

shutout_team_qtr<-qtr_pts%>%
  filter(season==seasonnum2)%>%
  group_by(team)%>%
  summarise(shutout_qtr_p=sum(shutout)/(weeknum2*4))

#防守组、特勤组TD,kickoff的posteam是接球的一方#####
DEF_SP_TD<-pbp%>%
  filter(season>=seasonnum1, season<=seasonnum2, week>=weeknum1, week<=weeknum2, 
         touchdown==1, posteam!=td_team,play_type!="kickoff") %>%
  select(game_id,defteam,qtr,desc,play_id, season, week, season_type, time, 
         posteam, td_team,  play_type, play_type_nfl,touchdown)%>%
  collect()
DEF_SP_TD03<-DEF_SP_TD%>%
  group_by(season,team=td_team)%>%
  summarise(DEF_TDS=sum(touchdown))

KICKOFF_TD<-pbp%>%
  filter(season>=seasonnum1, season<=seasonnum2, week>=weeknum1, week<=weeknum2, 
         touchdown==1,play_type=="kickoff") %>%
  select(game_id,defteam,qtr,desc,play_id, season, week, season_type, time, 
         posteam, td_team,  play_type, play_type_nfl,touchdown)%>%
  collect()

#进攻组每个drive的平均得分####
OFF_PTS<-pbp%>%
  filter(season>=seasonnum1, season<=seasonnum2, week>=weeknum1, week<=weeknum2, 
         drive_ended_with_score==1,play_type!="kickoff",!is.na(posteam),
         fixed_drive_result!="Safety")%>%
  select(season,week,game_id,team=posteam,desc,play_type,fixed_drive,fixed_drive_result,
         posteam_score,posteam_score_post,drive_play_count)%>%
  collect()
OFF_PTS%>%
  group_by(fixed_drive_result)%>%
  tally()

test<-pbp%>%
  filter(game_id=="2022_02_MIA_BAL")%>%
  filter(fixed_drive==1)%>%
  select(season,week,game_id,team=posteam,desc,play_type,fixed_drive,fixed_drive_result,
         posteam_score,posteam_score_post,drive_ended_with_score,drive_play_count)%>%
  collect()

OFF_PTS_1<-OFF_PTS%>%
  #filter(season==seasonnum2)%>%
  group_by(season,week,game_id,team,fixed_drive,fixed_drive_result)%>%
  summarise(drive_pts=max(posteam_score_post)-min(posteam_score))
OFF_PTS_1<-OFF_PTS_1%>%
  filter(drive_pts>1)
OFF_PTS_1%>%
  group_by(drive_pts)%>%
  tally()

OFF_PTS_team<-OFF_PTS_1%>%
  group_by(season,team)%>%
  summarise(offpts=sum(drive_pts))

OFF_PTS_team<-left_join(OFF_PTS_team,drive_list,by=c("season","team"))
OFF_PTS_team<-OFF_PTS_team%>%
  mutate(pts_pdrive=offpts/off_drives)



#绘图####
total_season%>%
  ggplot(aes(x=season, y=pts)) +
  geom_point(size=5,shape=21,fill="red",alpha=0.25) +
  geom_line(size=1) +
  scale_x_continuous(
    breaks = c(2018:2022),
    minor_breaks=NULL
  )+
  scale_y_continuous()+
  theme_bw()+
  labs(x = "SEASON",
       y = "Total Points in first 5 weeks",
       title = "Total Points in first 5 weeks Each Season",
       #subtitle = "2022 Season When Either Team Has WP Larger Than 88%",
       caption = "By ak47twq")+
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12),
        axis.text.y = element_text( size = 10),
        axis.text.x = element_text( size = 10),
        axis.title = element_text(size = 12))
#保存图像
ggsave(glue("Total Points in first 5 week {seasonnum2}.png"), width = 200, height = 200, 
       units = "mm", dpi = 600)

ggplot(total_season, aes(x=season)) + 
  geom_line(aes(y = TD, colour = "TD"),size=1) + 
  geom_line(aes(y = FGS, colour = "FGS"),size=1)+
  geom_line(aes(y = PUNTS, colour = "PUNTS"),size=1)+
  geom_line(aes(y = TOS, colour = "TOS"),size=1)+
  geom_point(aes(y = TD),size=5,shape=21,fill="red",alpha=0.25) +
  geom_point(aes(y = FGS),size=5,shape=21,fill="red",alpha=0.25) +
  geom_point(aes(y = PUNTS),size=5,shape=21,fill="red",alpha=0.25) +
  geom_point(aes(y = TOS),size=5,shape=21,fill="red",alpha=0.25) +
  scale_x_continuous(
    breaks = c(2018:2022),
    minor_breaks=NULL
  )+
  scale_y_continuous()+
  theme_bw()+
  labs(x = "SEASON",
       y = "Number",
       title = "Total something in first 5 weeks Each Season",
       subtitle = "Touchdowns, Field Goal Atts, Punts and Turnovers",
       caption = "By ak47twq")+
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12),
        axis.text.y = element_text( size = 10),
        axis.text.x = element_text( size = 10),
        axis.title = element_text(size = 12))

ggsave(glue("Total TDs in first 5 week {seasonnum2}.png"), width = 220, height = 200, 
       units = "mm", dpi = 600)

xlim1=max(shutout_team_qtr$shutout_qtr_p)+0.05
ggplot(shutout_team_qtr, aes(x=shutout_qtr_p, y=fct_reorder(team, -shutout_qtr_p))) + 
  geom_bar(stat = "identity",aes(fill=team,color=team),width = 0.8) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl() +
  nflplotR::scale_y_nfl(size=20) +
  #geom_vline(xintercept = 0.5, linetype = "dashed")+
  geom_vline(xintercept = mean(shutout_team_qtr$shutout_qtr_p), linetype = "dashed",color="red")+
  expand_limits(x = c(0, xlim1))+
  scale_x_continuous(breaks=seq(0,1,0.05),minor_breaks = NULL,expand=c(0,0))+
  theme_bw()+
  labs(x = "Single Qtr shutout PCT.",
       title = glue("Single Qtr shutout PCT each team thru. season {seasonnum2} Week {weeknum2}"),
       caption = "By ak47twq") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12),
        axis.text.y = element_text( face = "bold", size = 12),
        axis.text.x = element_text( size = 10),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        panel.grid.major.x = element_line(size = 0.75),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1))+
  nflplotR::theme_y_nfl()

ggsave(glue("team_shutout_qtr_w{weeknum2}.png"), width = 200, height = 300, 
       units = "mm", dpi = 600)

#绘图进攻组队伍####
plot<-OFF_PTS_team%>%
  filter(season==2022)
xlim1=max(plot$pts_pdrive)+0.05
ggplot(plot, aes(x=pts_pdrive, y=fct_reorder(team, pts_pdrive))) + 
  geom_bar(stat = "identity",aes(fill=team,color=team),width = 0.8) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl() +
  nflplotR::scale_y_nfl(size=20) +
  #geom_vline(xintercept = 0.5, linetype = "dashed")+
  geom_vline(xintercept = mean(plot$pts_pdrive), linetype = "dashed",color="red")+
  expand_limits(x = c(0, xlim1))+
  scale_x_continuous(breaks=seq(0,7,0.5),minor_breaks = NULL,expand=c(0,0))+
  theme_bw()+
  labs(x = "Points Scored Per Drive.",
       title = glue("PTS per Drive each team thru. season {seasonnum2} Week {weeknum2}"),
       caption = "By ak47twq") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12),
        axis.text.y = element_text( face = "bold", size = 12),
        axis.text.x = element_text( size = 10),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        panel.grid.major.x = element_line(size = 0.75),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1))+
  nflplotR::theme_y_nfl()

ggsave(glue("team_ptsperdrive_w{weeknum2}.png"), width = 200, height = 300, 
       units = "mm", dpi = 600)

plot<-total_season_off%>%
  filter(season==2022)
xlim1=max(plot$Score_p)+0.05
ggplot(plot, aes(x=Score_p, y=fct_reorder(team, Score_p))) + 
  geom_bar(stat = "identity",aes(fill=team,color=team),width = 0.8) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl() +
  nflplotR::scale_y_nfl(size=20) +
  geom_vline(xintercept = 0.5, linetype = "dashed")+
  geom_vline(xintercept = mean(plot$Score_p), linetype = "dashed",color="red")+
  expand_limits(x = c(0, xlim1))+
  scale_x_continuous(breaks=seq(0,1,0.05),minor_breaks = NULL,expand=c(0,0))+
  theme_bw()+
  labs(x = "Score PCT.",
       title = glue("Score PCT each team thru. season {seasonnum2} Week {weeknum2}"),
       caption = "By ak47twq") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12),
        axis.text.y = element_text( face = "bold", size = 12),
        axis.text.x = element_text( size = 10),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        panel.grid.major.x = element_line(size = 0.75),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1))+
  nflplotR::theme_y_nfl()

ggsave(glue("team_Score_w{weeknum2}.png"), width = 200, height = 300, 
       units = "mm", dpi = 600)

xlim1=max(plot$td_p)+0.05
ggplot(plot, aes(x=td_p, y=fct_reorder(team, td_p))) + 
  geom_bar(stat = "identity",aes(fill=team,color=team),width = 0.8) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl() +
  nflplotR::scale_y_nfl(size=20) +
  #geom_vline(xintercept = 0.5, linetype = "dashed")+
  geom_vline(xintercept = mean(plot$td_p), linetype = "dashed",color="red")+
  expand_limits(x = c(0, xlim1))+
  scale_x_continuous(breaks=seq(0,1,0.05),minor_breaks = NULL,expand=c(0,0))+
  theme_bw()+
  labs(x = "Touchdown PCT.",
       title = glue("TD PCT each team thru. season {seasonnum2} Week {weeknum2}"),
       caption = "By ak47twq") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12),
        axis.text.y = element_text( face = "bold", size = 12),
        axis.text.x = element_text( size = 10),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        panel.grid.major.x = element_line(size = 0.75),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1))+
  nflplotR::theme_y_nfl()

ggsave(glue("team_off_td_w{weeknum2}.png"), width = 200, height = 300, 
       units = "mm", dpi = 600)

xlim1=max(plot$fg_p)+0.05
ggplot(plot, aes(x=fg_p, y=fct_reorder(team, fg_p))) + 
  geom_bar(stat = "identity",aes(fill=team,color=team),width = 0.8) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl() +
  nflplotR::scale_y_nfl(size=20) +
  #geom_vline(xintercept = 0.5, linetype = "dashed")+
  geom_vline(xintercept = mean(plot$fg_p), linetype = "dashed",color="red")+
  expand_limits(x = c(0, xlim1))+
  scale_x_continuous(breaks=seq(0,1,0.05),minor_breaks = NULL,expand=c(0,0))+
  theme_bw()+
  labs(x = "Field Goal Attempt PCT.",
       title = glue("FGA PCT each team thru. season {seasonnum2} Week {weeknum2}"),
       caption = "By ak47twq") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12),
        axis.text.y = element_text( face = "bold", size = 12),
        axis.text.x = element_text( size = 10),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        panel.grid.major.x = element_line(size = 0.75),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1))+
  nflplotR::theme_y_nfl()

ggsave(glue("team_off_FGa_w{weeknum2}.png"), width = 200, height = 300, 
       units = "mm", dpi = 600)

xlim1=max(plot$to_p)+0.05
ggplot(plot, aes(x=to_p, y=fct_reorder(team, -to_p))) + 
  geom_bar(stat = "identity",aes(fill=team,color=team),width = 0.8) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl() +
  nflplotR::scale_y_nfl(size=20) +
  geom_vline(xintercept = mean(plot$to_p), linetype = "dashed",color="red")+
  expand_limits(x = c(0, xlim1))+
  scale_x_continuous(breaks=seq(0,1,0.05),minor_breaks = NULL,expand=c(0,0))+
  theme_bw()+
  labs(x = "Turnover PCT.",
       title = glue("Turnover PCT each team thru. season {seasonnum2} Week {weeknum2}"),
       caption = "By ak47twq") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12),
        axis.text.y = element_text( face = "bold", size = 12),
        axis.text.x = element_text( size = 10),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        panel.grid.major.x = element_line(size = 0.75),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1))+
  nflplotR::theme_y_nfl()

ggsave(glue("team_Turnover_w{weeknum2}.png"), width = 200, height = 300, 
       units = "mm", dpi = 600)

xlim1=max(plot$red_p)+0.05
ggplot(plot, aes(x=red_p, y=fct_reorder(team, red_p))) + 
  geom_bar(stat = "identity",aes(fill=team,color=team),width = 0.8) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl() +
  nflplotR::scale_y_nfl(size=20) +
  #geom_vline(xintercept = 0.5, linetype = "dashed")+
  geom_vline(xintercept = mean(plot$red_p), linetype = "dashed",color="red")+
  expand_limits(x = c(0, xlim1))+
  scale_x_continuous(breaks=seq(0,1,0.05),minor_breaks = NULL,expand=c(0,0))+
  theme_bw()+
  labs(x = "Redzone PCT.",
       title = glue("Redzone PCT each team thru. season {seasonnum2} Week {weeknum2}"),
       caption = "By ak47twq") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12),
        axis.text.y = element_text( face = "bold", size = 12),
        axis.text.x = element_text( size = 10),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        panel.grid.major.x = element_line(size = 0.75),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1))+
  nflplotR::theme_y_nfl()

ggsave(glue("team_redzone_w{weeknum2}.png"), width = 200, height = 300, 
       units = "mm", dpi = 600)

xlim1=max(plot$redtd_p)+0.05
ggplot(plot, aes(x=redtd_p, y=fct_reorder(team, redtd_p))) + 
  geom_bar(stat = "identity",aes(fill=team,color=team),width = 0.8) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl() +
  nflplotR::scale_y_nfl(size=20) +
  geom_vline(xintercept = 0.5, linetype = "dashed")+
  geom_vline(xintercept = mean(plot$redtd_p), linetype = "dashed",color="red")+
  expand_limits(x = c(0, xlim1))+
  scale_x_continuous(breaks=seq(0,1,0.05),minor_breaks = NULL,expand=c(0,0))+
  theme_bw()+
  labs(x = "RedzoneTD PCT.",
       title = glue("RedzoneTD PCT each team thru. season {seasonnum2} Week {weeknum2}"),
       caption = "By ak47twq") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12),
        axis.text.y = element_text( face = "bold", size = 12),
        axis.text.x = element_text( size = 10),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        panel.grid.major.x = element_line(size = 0.75),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1))+
  nflplotR::theme_y_nfl()

ggsave(glue("team_redzonetd_w{weeknum2}.png"), width = 200, height = 300, 
       units = "mm", dpi = 600)

xlim1=max(plot$outredtd_p)+0.05
ggplot(plot, aes(x=outredtd_p, y=fct_reorder(team, outredtd_p))) + 
  geom_bar(stat = "identity",aes(fill=team,color=team),width = 0.8) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl() +
  nflplotR::scale_y_nfl(size=20) +
  #geom_vline(xintercept = 0.5, linetype = "dashed")+
  geom_vline(xintercept = mean(plot$outredtd_p), linetype = "dashed",color="red")+
  expand_limits(x = c(0, xlim1))+
  scale_x_continuous(breaks=seq(0,1,0.05),minor_breaks = NULL,expand=c(0,0))+
  theme_bw()+
  labs(x = "OUT RedzoneTD PCT.",
       title = glue("OUT RedzoneTD PCT each team thru. season {seasonnum2} Week {weeknum2}"),
       caption = "By ak47twq") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12),
        axis.text.y = element_text( face = "bold", size = 12),
        axis.text.x = element_text( size = 10),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        panel.grid.major.x = element_line(size = 0.75),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1))+
  nflplotR::theme_y_nfl()

ggsave(glue("team_out_redzonetd_w{weeknum2}.png"), width = 200, height = 300, 
       units = "mm", dpi = 600)