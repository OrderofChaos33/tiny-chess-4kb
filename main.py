import os,json,urllib.request as U
V=dict(zip('PNBRQK',(100,300,320,500,950,0)))
T={'P':'kkkkuuuummoqllmpkkkoljiklmmgkkkk','N':'aceecgkkekmnelnoeknoelmncgklacee'}
DN=(-33,-31,-18,-14,14,18,31,33);DB=(-17,-15,15,17);DR=(-16,-1,1,16);DQ=DB+DR
D={'N':(DN,0),'B':(DB,1),'R':(DR,1),'Q':(DQ,1),'K':(DQ,0)}
B=['.']*128;W=1;CA=set();EP=-1;I=10**7
def ev():
 s=0
 for q in range(128):
  p=B[q]
  if p!='.'and not q&0x88:
   u=p.upper();r,f=q>>4,q&7;f=min(f,7-f);v=V[u]+(ord(T.get(u,'k'*32)[(r if p.isupper()else 7-r)*4+f])-107)*5;s+=v if p.isupper()else-v
 return s
def atk(s,w):
 for d in(15,17):
  t=s+d if w else s-d
  if not t&0x88 and B[t]==('P'if w else'p'):return 1
 for ds,pc in((DN,'N'),(DQ,'K')):
  pc=pc if w else pc.lower()
  for d in ds:
   t=s+d
   if not t&0x88 and B[t]==pc:return 1
 for ds,pp in((DB,'BQ'),(DR,'RQ')):
  for d in ds:
   t=s+d
   while not t&0x88:
    p=B[t]
    if p!='.':
     if p.isupper()==w and p.upper()in pp:return 1
     break
    t+=d
 return 0
def gen(cap=0):
 ms=[];ap=ms.append;w=W
 for s in range(128):
  p=B[s]
  if s&0x88 or p=='.'or p.isupper()!=w:continue
  u=p.upper()
  if u=='P':
   fw=-16 if w else 16;r=s>>4;pr=r==(1 if w else 6)
   t=s+fw
   if B[t]=='.'and not cap:
    ms+=[(s,t,x)for x in'qrbn']if pr else[(s,t,'')]
    if not pr and r==(6 if w else 1)and B[s+2*fw]=='.':ap((s,s+2*fw,''))
   for d in((-15,-17)if w else(15,17)):
    t=s+d
    if t&0x88:continue
    q=B[t]
    if q!='.'and q.isupper()!=w or t==EP:ms+=[(s,t,x)for x in'qrbn']if pr else[(s,t,'')]
  else:
   ds,sl=D[u]
   for d in ds:
    t=s+d
    while not t&0x88:
     q=B[t]
     if q=='.':
      if not cap:ap((s,t,''))
     elif q.isupper()!=w:ap((s,t,''))
     if q!='.'or not sl:break
     t+=d
 if not cap:
  for cr,ks,st in(('K',116,1),('Q',116,-1),('k',4,1),('q',4,-1)):
   if cr in CA and B[ks+st]==B[ks+2*st]=='.'and(st>0 or B[ks+3*st]=='.')and not any(atk(x,not w)for x in(ks,ks+st,ks+2*st)):ap((ks,ks+2*st,''))
 return ms
def mk(m):
 global CA,EP,W
 f,t,pr=m;p=B[f];cap=B[t];es=-1;u=p.upper()
 if u=='P'and t==EP and cap=='.':es=t+16 if p.isupper()else t-16;B[es]='.'
 old=(cap,CA,EP,es);CA=set(CA);B[f]='.'
 B[t]=(pr.upper()if p.isupper()else pr)if pr else p
 EP=(f+t)//2 if u=='P'and abs(t-f)==32 else -1
 if u=='K':
  if abs(t-f)==2:
   rf,rt=(t+1,t-1)if t>f else(t-2,t+1);B[rt]=B[rf];B[rf]='.'
  CA-={'K','Q'}if p.isupper()else{'k','q'}
 CA-={cr for sq,cr in zip((112,119,0,7),'QKqk')if f==sq or t==sq}
 W=not W
 return old
def un(m,old):
 global CA,EP,W
 f,t,pr=m;cap,CA,EP,es=old;W=not W;p=B[t]
 B[f]=('P'if p.isupper()else'p')if pr else p;B[t]=cap
 if es>=0:B[es]='p'if W else'P'
 if B[f].upper()=='K'and abs(t-f)==2:
  rf,rt=(t+1,t-1)if t>f else(t-2,t+1);B[rf]=B[rt];B[rt]='.'
def om(ms):
 return sorted(ms,key=lambda m:(10*V[B[m[1]].upper()]-V[B[m[0]].upper()]//10 if B[m[1]]!='.'else 1005 if m[1]==EP else 0),reverse=True)
def sr(d,a,b,ply):
 w=W;chk=atk(B.index('K'if w else'k'),not w)
 if d<=0:
  st=ev()*(1 if w else-1)
  if not chk:
   if st>=b:return st
   if st>a:a=st
 bs=-I
 for m in om(gen(d<=0 and not chk)):
  o=mk(m)
  if atk(B.index('K'if w else'k'),W):un(m,o);continue
  s=-sr(d-1,-b,-a,ply+1);un(m,o)
  if s>bs:bs=s
  if s>a:a=s
  if a>=b:break
 if bs>-I:return bs
 return(ply-99999)if chk else st if d<=0 else 0
def parse(fen):
 global B,W,CA,EP
 B=['.']*128;pt=fen.split();r=f=0
 for c in pt[0]:
  if c=='/':r+=1;f=0
  elif c.isdigit():f+=int(c)
  else:B[r*16+f]=c;f+=1
 W=pt[1]=='w';CA={*pt[2]}-{'-'}
 EP=-1 if pt[3]=='-'else(8-int(pt[3][1]))*16+ord(pt[3][0])-97
def nm(s):return chr(97+(s&7))+str(8-(s>>4))
def bm(fen):
 parse(fen);a=-I;best=None
 for m in om(gen(0)):
  o=mk(m)
  if atk(B.index('K'if not W else'k'),W):un(m,o);continue
  s=-sr(3,-I,I,1);un(m,o)
  if s>a:a=s;best=m
 return nm(best[0])+nm(best[1])+best[2]
def post(p,d):
 q=U.Request(os.environ['MAKEFUN_ENV_URL']+p,data=json.dumps(d).encode(),headers={'Content-Type':'application/json'});return json.loads(U.urlopen(q).read())
while 1:
 g=post('/game',{})
 if g['status']=='done':break
 m=bm(g['fen'])
 post('/move',{'game':g['game'],'move':m})