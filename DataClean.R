library(dplyr)

INV1 <- read.table("C:/Users/193344/Desktop/Status Inventory/INVSIRK.txt",header=F,sep="\t")
INV2 <- read.table("C:/Users/193344/Desktop/Status Inventory/INVSIRO.txt",header=F,sep="\t")
headers <- read.csv("C:/Users/193344/Desktop/Status Inventory/headers.csv",header=F,stringsAsFactors=F)
status <- read.csv("C:/Users/193344/Desktop/Status Inventory/status.csv",header=T,stringsAsFactors=F)
armast <- read.csv("C:/Users/193344/Desktop/Status Inventory/ARMASTER.csv",header=T,stringsAsFactors=F)

INV <- rbind(INV1,INV2)
colnames(INV) <- headers[1,]

INV$"LST ACT DT" <- as.numeric(as.Date(INV$"LST ACT DT",format="%m/%d/%Y"))
INV$"LST DIALER UPLOAD DT" <- as.numeric(as.Date(INV$"LST DIALER UPLOAD DT",format="%m-%d-%Y"))
INV$"LAST ACT" <- as.numeric(as.Date(INV$"LAST ACT",format="%m/%d/%Y"))

mgr <- armast %>%
  select(c(1,2)) %>%
  rename("manager"=MGR,"DSK #" =DESK)
library(plyr)
INV <- join(INV,mgr,by="DSK #",match="first")
detach("package:plyr", unload=TRUE)



dsk <- armast %>%
  select(c(1,3)) %>%
  rename(MGR=DESK)
INV <- rename(INV, MGR=manager)
library(plyr)
INV <- join(INV,dsk,by="MGR",match="first")
detach("package:plyr", unload=TRUE)




INV <-mutate(INV,MAX = ifelse(STAT=="UNA"| (is.na("LST ACT DT")&is.na("LST DIALER UPLOAD DT")&is.na("LAST ACT")),
                          100000,apply(INV[,8:10], 1, function(x) max(x[x != 9],na.rm=TRUE)))
  )


col <- armast %>%
  select(c(1,4,5)) %>%
  rename("DSK #"=DESK)
library(plyr)
INV <- join(INV,col,by="DSK #",match="first")
detach("package:plyr", unload=TRUE)



INVADM <- filter(INV,DEPT=="ADM")
INVOTH <- filter(INV,DEPT!="ADM")

ADMstats <- select(status,c(1,3)) %>%
  rename(Allowable_Days=ADM,STAT=STATUS)

OTHstats <- select(status,c(1,2)) %>%
  rename(Allowable_Days=COL,STAT=STATUS)

library(plyr)
INVADM <- join(INVADM,ADMstats,by="STAT",match="first")
INVOTH <- join(INVOTH,OTHstats,by="STAT",match="first")
INV<-rbind(INVADM,INVOTH)
detach("package:plyr", unload=TRUE)
rownames(INV) <- NULL

INV <- mutate(INV, DYS=ifelse(MAX==100000,100000,as.numeric(Sys.Date()-1)-MAX
)
)
INV$Allowable_Days<-as.numeric(INV$Allowable_Days)
INV <- INV %>%
  mutate("Thirty_Days"=ifelse((DYS==""|DEPT=="AGY"),"",ifelse(Allowable_Days<30&DYS>30,1,0))) %>%
  mutate("Seven_Days"=ifelse((DYS==""|DEPT=="AGY"),"",ifelse(Allowable_Days<7&DYS>7,1,0))) %>%
  mutate("Action_Days"=ifelse((DYS==""|DEPT=="AGY"),"",ifelse(DYS>Allowable_Days,1,0)))


INV$MAX <- as.Date(INV$MAX,origin="1970-01-01")
INV$"LST ACT DT" <-as.Date(INV$"LST ACT DT",origin="1970-01-01")
INV$"LST DIALER UPLOAD DT" <- as.Date(INV$"LST DIALER UPLOAD DT",origin="1970-01-01")
INV$"LAST ACT" <- as.Date(INV$"LAST ACT",origin="1970-01-01")
INV$">2500"<-0
INV <- mutate(INV,STATUS=ifelse(is.na(Allowable_Days),1,0))
INV$DESK<-INV$"DSK #"

INV <- INV[,c(22,18,14,15,16,17,19,20,21,3,13,23,24,4,5,1,2,6,7,8,9,10,11,12)]

INV <- rename(INV,Manager=Employee)
INV$OFFICE <- as.factor(INV$OFFICE)
INV$DEPT <- as.factor(INV$DEPT)
INV$Manager <- as.factor(INV$Manager)

library(plyr)
INV$OFFICE <- revalue(INV$OFFICE,c("A"="Atlanta","B"="Columbus-2","K"="Knoxville","C"="Columbus","W"="Westlake"))

detach("package:plyr", unload=TRUE)


