-- Define the function to set the "updated_at" column
CREATE OR REPLACE FUNCTION set_updated_at_column() 
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = now();
    RETURN NEW; 
END;
$$ language 'plpgsql';

-- Create Triggers
CREATE TRIGGER set_users_updated_at BEFORE UPDATE ON "users" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
CREATE TRIGGER set_sessions_updated_at BEFORE UPDATE ON "sessions" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
CREATE TRIGGER set_tribes_updated_at BEFORE UPDATE ON "tribes" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
CREATE TRIGGER set_locations_updated_at BEFORE UPDATE ON "locations" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
CREATE TRIGGER set_workouts_updated_at BEFORE UPDATE ON "workouts" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
CREATE TRIGGER set_verbals_updated_at BEFORE UPDATE ON "verbals" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
CREATE TRIGGER set_results_updated_at BEFORE UPDATE ON "results" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
CREATE TRIGGER set_events_updated_at BEFORE UPDATE ON "events" FOR EACH ROW EXECUTE PROCEDURE set_updated_at_column();
