activityfeed
============

Demo application for an activity feed where users can get their list of activities (feed). In this list he could see
the activities from all his connections.

API
===

Restful API:

  * `POST /users/:uid/activities/` create new activity
  * `GET /users/:uid/activities/:aid` get an activity
  * `GET /users/:uid/activities/` get activity feed
  
Design
======

Model
-----

We have `User`s and `Activity` resources.

Database
--------

NoSQL schema to support scalability (Riak in combination with Redis could be used).

  * ActivitiesCollection `{key: uid, value: {pages: [pageid, ...]}}`
  * ActivitiesPage `{key: pageid, value: {activities: [{content: "..."}, ]}}`

`ActivitiesCollection` has references to the `ActivitiesPage`s. Each `ActivityPage` has a list of activities for the user.
When a page reaches a limit a new page is created to avoid loading all the list of activities.

Advantages
==========

  * Using the schema described above, the feed can generally be retreived with two simple queries by key.
  * Using a HiLo algorithm (a centralized ACID DB is needed for this, e.g. Redis) to generate the ids for
    activities can be used to find any activity in exactly 2 queries.
  * The documents can be stored in a distributed DB like Ryak.

Limitations
===========

  * With the current design there can be concurrency problems when saving the activitiescollection and acticitiespage data.
    To mitigate this problem some kind of concurrency control has to be added, e.g. CAS control.
  * In a tradicional activity feed, activities for users published to all its connections. With the current design
    this would require save the activity on all the activitiespages belonging to his connections. This can be done
    asynchronously.
