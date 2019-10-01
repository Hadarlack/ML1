--build table
create table tedEvent(
indexX int primary key,
eventX varchar(255)
)

--create sandbox for tries
select *
into tedEvent
from sandbox


--update events to groups
update tedEvent
set eventX='TED-Ed'
where eventX like 'TED-Ed%';

update tedEvent
set eventX='TED Fellows'
where eventX like 'TED Fellows%';

update tedEvent
set eventX='TED@'
where eventX like 'TED@%';

update tedEvent
set eventX='TED original'
where eventX like 'TED1%';

update tedEvent
set eventX='TED original'
where eventX like 'TED2%';

update tedEvent
set eventX='TEDActive'
where eventX like 'TEDActive%';

update tedEvent
set eventX='TEDGlobal'
where eventX like 'TEDGlobal%';

update tedEvent
set eventX='TEDMED'
where eventX like 'TEDMED%';

update tedEvent
set eventX='TEDSalon'
where eventX like 'TEDSalon%';

update tedEvent
set eventX='TEDWomen'
where eventX like 'TEDWomen%';

update tedEvent
set eventX='TEDx'
where eventX like 'TEDx%';

update tedEvent
set eventX='TEDYouth'
where eventX like 'TEDYouth%';

update tedEvent
set eventX='EG'
where eventX like 'EG%';

--very big division from here!
update tedEvent
set eventX='Small Events'
where eventX in ('INK Conference',
'RSA Animate',
'Chautauqua Institution',
'Business Innovation Factory',
'Full Spectrum Auditions',
'University of California',
'Gel Conference',
'Stanford University',
'DLD 2007',
'SoulPancake',
'Carnegie Mellon University',
'Royal Institution',
'Web 2.0 Expo 2008',
'Fort Worth City Council',
'Handheld Learning',
'Michael Howard Studios',
'LIFT 2007',
'AORN Congress',
'Toronto Youth Corps',
'Skoll World Forum 2007',
'The Do Lectures',
'World Science Festival',
'Princeton University',
'New York State Senate',
'Harvard University',
'BBC TV',
'Bowery Poetry Club',
'Global Witness HQ',
'Elizabeth G. Anderson School',
'Arbejdsglaede Live',
'Justice with Michael Sandel',
'DICE Summit 2010',
'NextGen:Charity',
'Taste3 2008',
'Serious Play 2008',
'Mission Blue II',
'Mission Blue Voyage',
'EG');


update tedEvent
set eventX='Small Events'
where eventX like 'Eric Whitacre%';

update tedEvent
set eventX='TED other'
where eventX in ('TED Residency',
'TED in the Field',
'TED Studio',
'TED Dialogues',
'TEDPrize@UN',
'TEDLagos Ideas Search',
'TEDNairobi Ideas Search',
'TED Prize Wish',
'TED Senior Fellows at TEDGlobal 2010',
'TED Talks Education',
'TED-Ed',
'TEDCity2.0',
'TED Fellows',
'TEDNYC',
'TEDYouth',
'TEDActive',
'TED Talks Live',
'TEDIndia 2009')


--count how much from each event
select eventX, count(*) as amount
from tedEvent
group by eventX
order by amount DESC;

select eventX
from tedEvent