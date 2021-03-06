## Snaplet-Tasks

Snap Framework ( http://snapframework.com ) support for command line
tasks akin to _Rake_ tasks from _Ruby On Rails_

Depends on Snap 0.7.* and <others>

## Installation

```
cabal install snaplet-tasks
```

### Integration

in coolapp.cabal

```
Build-depends:
  snaplet-tasks
```

in Application.hs

```
import           Snap.Snaplet.Tasks



data App = App
    { _heist :: Snaplet (Heist App)
    
    , _tasks :: Snaplet TasksSnaplet
    }
```

in Site.hs

```
app = do
  h  <- nestSnaplet "heist" heist $ do
            heistInit "resources/templates"
  t <- nestSnaplet  "tasks" tasks $
            tasksInit
  return $ App h  t
```

Because we need full app state at disposal in our tasks
the easiest way was to create them from normal handlers.

Thus, in order to run task - app has to be started.
It is comparable with Ruby On Rails Rake tasks where
whole app environment _can_ be loaded for task as well.
The only difference is that here in Snap - app also listens
on TCP for connections - and that *we utilize*.

There are two constriants to running tasks:

1. You can't run them remotely ( meaning that you can't fire app task handler remotely )
2. Task route is being _hashified_ and the only way to specify task is by using command line app arg switch _T_  ( ex. T mysupertask or T namespace:second:supertask ) The supplied name of task is being hashified again and thus matched with route that responds to exaclty that hash.

Example command line usage:

```
yourapp T db:migrate
```

Or if you want to run app for tasks on different port:

```
yourapp T db:migrate -p 1234
```

This implies that you have to have some tool/function to create
tasks so that they respond to hashified names. And indeed you have:

```
task :: String -> (Handler b v ()) -> (B.ByteString, Handler b v ())
```

This function takes name of the task ( ex. "db:migrate" ) and a handler
and returns a tuple that you can use defining your app routes.

