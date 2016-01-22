DO $$
    DECLARE
        fid VARCHAR;
        keepID BIGINT;
        oldIDs users.id%type;
BEGIN
    FOR fid IN (select facebook_id as fid from users where facebook_id is not null group by facebook_id having count(*)>1)
    LOOP
        RAISE NOTICE 'Processing %', fid;
        keepID := (select MAX(id) from users where facebook_id = fid);
        select id into oldIDs from users where facebook_id = fid EXCEPT select keepID;
        RAISE NOTICE 'Keeping %', keepID;
        RAISE NOTICE 'Removing %', oldIDs;

        BEGIN
            update verbals set "user" = keepID where "user" in (select oldIDs);
        EXCEPTION
            WHEN unique_violation THEN
                delete from verbals where "user" in (select oldIDs);
        END;

        BEGIN
            update results set "user" = keepID where "user" in (select oldIDs);
        EXCEPTION
            WHEN unique_violation THEN
                delete from results where "user" in (select oldIDs);
        END;

        delete from users where id in (select oldIDs);
    END LOOP;
    RETURN;
END;
$$ LANGUAGE plpgsql;
