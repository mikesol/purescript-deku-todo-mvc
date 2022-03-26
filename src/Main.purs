module Main where

import Prelude

import Data.Filterable (filterMap)
import Data.Foldable (for_)
import Data.Map (delete, empty, insert, singleton, update)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Deku.Change (change)
import Deku.Control.Functions ((%%>))
import Deku.Graph.Attribute (cb)
import Deku.Graph.DOM (AsSubgraph(..), SubgraphSig, subgraph, xsubgraph, (:=))
import Deku.Graph.DOM as D
import Deku.Pursx ((~!), pursx)
import Deku.Toplevel ((🚀))
import Effect (Effect)
import Type.Proxy (Proxy)
import Web.DOM.Element (fromEventTarget)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromElement, value)

-- | The type of our todo element
type Todo = { completed :: Boolean, text :: String }
-- | Filtering options for the todo list
data FilterOption = All | Active | Completed
-- | The main actions we can do via the toplevel UI
data MainUIAction
  = AddTodo -- add whatever is in the current input text as a todo
  | ChangeText String -- change the current input text
  | FilterTodo FilterOption -- filter todo list
  | TodoAction TodoAction -- sent from children
-- | Action raised from children
data TodoAction
  = SetTodo Int Boolean -- set this todo to completed or not
  | RemoveTodo Int -- remove this todo

-- css
-- when we are displaying done as an option (meaning we're not done)
doneButtonCss :: String
doneButtonCss = "flex-no-shrink p-2 ml-4 mr-2 border-2 rounded hover:text-white text-green-400 border-green-500 hover:bg-green-400"
-- when we are display a todo item is done
doneTextCss :: String
doneTextCss = "w-full line-through text-green"
-- when we are displaying not done as an option (meaning we're done)
notDoneButtonCss :: String
notDoneButtonCss = "flex-no-shrink p-2 ml-4 mr-2 border-2 rounded hover:text-white text-gray-300 border-gray-400 hover:bg-gray-400"
-- when we are display a todo item is not done
notDoneTextCss :: String
notDoneTextCss = "w-full text-green"
-- filter button is selected
filterButtonSelected :: String
filterButtonSelected = "p-2 flex-1 border-2 rounded text-fuchsia-400	text-fuchsia-500 border-fuchsia-600 hover:text-white hover:bg-fuchsia-500"
-- filter button is selected
filterButtonNotSelected :: String
filterButtonNotSelected = "p-2 flex-1 border-2 rounded text-fuchsia-400	text-fuchsia-500 hover:text-white hover:bg-fuchsia-500"

-- | The main todo list container
-- | css from https://tailwindcomponents.com/component/todo-list-app
-- | each element between squigglies corresponds to something
-- | we will set in the PS code when we fill in our template
mainPursx = pursx :: Proxy
 """
<div class="h-100 w-full flex items-center justify-center bg-teal-lightest font-sans">
	<div class="bg-white rounded shadow p-6 m-4 w-full lg:w-3/4 lg:max-w-lg">
        <div class="mb-4">
            <h1 class="text-2xl text-gray-900">Todo List</h1>
            <div class="flex mt-4">
                <input ~input~ class="shadow appearance-none border rounded w-full py-2 px-3 mr-4 text-gray-700" placeholder="Add Todo" />
                <button ~addTodo~ class="flex-no-shrink p-2 border-2 rounded text-blue-400 border-blue-500 hover:text-white hover:bg-blue-500">Add</button>
            </div>
        </div>
        <div>
          ~todos~
        </div>
        <div class="flex mt-4">
            <button ~showDone~ class="p-2 flex-1 border-2 rounded text-fuchsia-400	text-fuchsia-500 hover:text-white hover:bg-fuchsia-500">Show Done</button>
            <button ~showNotDone~ class="p-2 flex-1 border-2 rounded text-fuchsia-400	text-fuchsia-500 hover:text-white hover:bg-fuchsia-500">Show Not Done</button>
            <button ~showAll~ class="p-2 flex-1 border-2 rounded text-fuchsia-400	text-fuchsia-500 border-fuchsia-600 hover:text-white hover:bg-fuchsia-500">Show All</button>
        </div>
    </div>
</div>
"""

-- | The program's main function
-- | A thin wrapper around 🚀
-- | Each argument to 🚀 will be described separately
main :: Effect Unit
main =
  -- first argument to 🚀 is the pursx + any values
  -- that need to be filled into to the pursx template
  -- these values are set between ~ (squgglies)
  -- if you look at the main template, you'll see the following keys:
  -- input, addTodo, showDone, showNotDone, showAll, todos
  -- these keys are the keys of the record below
  -- those keys that are in elements take attributes (like input)
  -- whereas those keys that are between elements take new elements (like todos)
  ( \push -> (mainPursx ~!
      let
        -- a helper function for our filter
        showTodos ac = D.button'attr [ D.OnClick := cb (\_ -> push $ FilterTodo ac) ]
      in
        -- here we add the listener to change the text on input
        { input: D.input'attr
            [ D.OnInput := cb \e -> for_
                ( target e
                    >>= fromEventTarget
                    >>= fromElement
                )
                ( value
                    >=> push <<< ChangeText
                )
            ]
        -- here we add the listener that actually adds the todo
        , addTodo: D.button'attr [ D.OnClick := cb \_ -> push AddTodo ]
        -- here we add the listeners for the filter buttons
        , showDone: showTodos Completed
        , showNotDone: showTodos Active
        , showAll: showTodos All
        -- here we add the actually todo list - it is empty at first
        -- we pass the todo list a version of our push function
        -- that responds to TodoActions
        -- this is the equivalent of queries and `raise` in halogen
        , todos: subgraph empty (AsSubgraph (todo (TodoAction >>> push)))
        }
        -- this is our state, which we must return as the
        -- second part of the tuple
        -- if we don't use a state, we can make this `unit`
        -- here, we keep track of the total number of
        -- items added, the current text, what filter is
        -- applied and what todos are present
  ) /\ ({ nItems: 0, text: "", filter: All, todos: empty }))
       -- the second argument to 🚀 receives the event
       -- that was pushed as well as the current state
       -- we always must return a new state, which could just
       -- be the current state if we don't need to change it
       -- this is wrapped in the MDOM monad, which is created
       -- using `pure` or `change` (see below)
    🚀 \push state -> case push of
        -- add a todo
        AddTodo ->
          -- only add if the string has something in it
          if state.text /= "" then
            let
              -- increment the counter
              np1 = state.nItems + 1
              -- construct the new todo
              new = { text: state.text, completed: false }
            in
              -- the record sent to change is always namespaced
              -- via a barlow lens
              -- for top-level elements, the first part of the
              -- lens is _always_ root
              change
                -- the path to our todos is
                -- - root (required at the toplevel)
                -- - psx (created when we use ~!)
                -- - todos (the value between the sqigglies in the template)
                { "root.psx.todos":
                    -- send the new todo to the todo list
                    xsubgraph (singleton np1 (Just new))
                , "root.psx.input":
                    -- clear the input
                    D.input'attr [ D.Value := "" ]
                    -- set the new state
                } $> state { nItems = np1, text = "", todos = insert np1 new state.todos }
          -- if the text is empty, it's a no-op
          else pure state
        -- handle changing text
        ChangeText txt -> pure $ state { text = txt }
        -- handle the filter buttons
        FilterTodo All ->
          change
            -- changes to subgraphs work the following way
            -- Just -> Just: modify the value at an index
            -- Just -> Nothing: remove the value at an index
            -- Nothing -> Just: add a value at the index
            -- Nothing -> Nothing: no-op
            -- here, we set everything to "Just" because we
            -- are filtering for all
            { "root.psx.todos": xsubgraph (map Just state.todos)
            , "root.psx.showDone": D.button'attr [ D.Class := filterButtonNotSelected ]
            , "root.psx.showAll": D.button'attr [ D.Class := filterButtonSelected ]
            , "root.psx.showNotDone": D.button'attr [ D.Class := filterButtonNotSelected ]
            } $> state { filter = All }
        FilterTodo Active ->
          change
            -- here, we set actives to `Just` and completed to `Nothing`
            { "root.psx.todos": xsubgraph (filterMap (\x@{ completed } -> if not completed then Just (Just x) else (Just Nothing)) state.todos)
            , "root.psx.showDone": D.button'attr [ D.Class := filterButtonNotSelected ]
            , "root.psx.showAll": D.button'attr [ D.Class := filterButtonNotSelected ]
            , "root.psx.showNotDone": D.button'attr [ D.Class := filterButtonSelected ]
            } $> state { filter = Active }
        FilterTodo Completed ->
          change
            -- here, we set completed to `Just` and actives to `Nothing`
            { "root.psx.todos": xsubgraph (filterMap (\x@{ completed } -> if completed then Just (Just x) else (Just Nothing)) state.todos)
            , "root.psx.showDone": D.button'attr [ D.Class := filterButtonSelected ]
            , "root.psx.showAll": D.button'attr [ D.Class := filterButtonNotSelected ]
            , "root.psx.showNotDone": D.button'attr [ D.Class := filterButtonNotSelected ]
            } $> state { filter = Completed }
        -- these are the actions that are raised from child components
        -- for set todo, we flip the value of completed
        -- note that this is _not_ propagated down to the subgraph
        -- it doesn't need to be, as the subgraph keeps track
        -- of its own state
        TodoAction (SetTodo x c) ->
          pure $ state { todos = update (\{ text } -> Just { text, completed: c }) x state.todos }
        -- delete a todo
        -- here, we propagate `Nothing` down to the deleted subgraph
        -- which removes it from the list
        TodoAction (RemoveTodo x) ->
          change
            { "root.psx.todos": xsubgraph (singleton x Nothing)
            } $> state { todos = delete x state.todos }

-- | An individual todo list item
-- | css from https://tailwindcomponents.com/component/todo-list-app
-- | each element between squigglies corresponds to something
-- | we will set in the PS code when we fill in our template
itemPursx = pursx :: Proxy
 """
<div class="flex mb-4 items-center">
    <p ~todoText~>~text~</p>
    <button ~doneButton~>~doneText~</button>
    <button ~removeButton~ class="flex-no-shrink p-2 ml-2 border-2 rounded text-red-300 border-red-400 hover:text-white hover:bg-red-400">Remove</button>
</div>
"""

-- the internal action for a component
data TodoInternalAction = FlipCompletion Boolean

-- | Subgraph components look very much like toplevel graphs
-- | the two are subtly different, which I'll get into below
-- | this subgraph creator takes an additional `raise` parameter,
-- | which is the communication channel with the parent component
-- | then, it _always_ takes as its first argument the type by
-- | which it is indexed. That is `Int` for this subgraph. In the
-- | main component, we can see `Int`-s being used as keys in the
-- | subgraph map, and those are sent here.
-- | Note that keys must implement `Ord` and `Hashable`.
todo
  :: (TodoAction -> Effect Unit)
  -> SubgraphSig Int Todo TodoInternalAction
todo raise n =
  let
    -- a helper function that sends completion information to
    -- the parent via `raise` and flips the completion status
    -- locally
    btnClick push completed = cb \_ ->
      let nc = not completed
      in raise (SetTodo n nc) *>  push (FlipCompletion nc)
  in
    -- The first argument to `~!` gets an additional parameter
    -- compared to 🚀. This additional parameter is the incoming
    -- information. In this case, that is the todo text and
    -- completion status. Like the toplevel component, it also
    -- receives an event pusher that can be sent back up to the
    -- top component and/or propagated down to children and/or
    -- used internally. Here, it is just used internally on
    -- the click listeners.
    ( \{ completed, text } push ->
        -- like before, we use our pursx template
        ( itemPursx ~!
            -- this is the text of the todo item
            { text: D.text text
            -- this is the styling for the text of the todo item
            , todoText: D.p'attr [D.Class := if completed then doneTextCss else notDoneTextCss]
            -- this is the text of the done button
            , doneText: D.text (if completed then "Not Done" else "Done")
            -- these are the attributes of the done button
            -- we set a click listener to set the todo status
            -- as well as some styling
            , doneButton: D.button'attr
                [ D.OnClick := btnClick push completed
                , D.Class := if completed then notDoneButtonCss else doneButtonCss ]
            -- these are the attributes of the remove button
            -- we set the lisetner that will remove the todo
            -- note that all we do in this case is raise,
            -- and the toplevel takes care of removing it from
            -- the list
            , removeButton: D.button'attr
              [ D.OnClick := cb \_ -> raise (RemoveTodo n) ]
            }
        -- the state is just the pusher
        ) /\ { push }
    -- the %%> operator is for loops that are
    -- _passive_ to their environment.
    -- That means that they don't respond to incoming changes to
    -- the environment.
    -- That makes the first parameter, `action`, _only_ the local action.
    -- Otherwise, it would be an `Either` and we would need to
    -- pattern match on information from the parent component
    -- (this is `input` in Halogen) _or_ our local information.
    ) %%> \action state ->
        case action of
          -- note that our barlow lenses do not start with
          -- `root`. This is because this is a subgraph. Subgraphs
          -- start directly at the child components
          FlipCompletion completed -> change
            -- set the text of the button bsed on the completion status
            { "psx.doneText": if completed then "Not Done" else "Done"
            -- set the css of the text based on the completion status
            , "psx.todoText": D.p'attr [D.Class := if completed then doneTextCss else notDoneTextCss]
            -- we change the click listener to flip the completion status
            , "psx.doneButton":
                D.button'attr [ D.OnClick := btnClick state.push completed, D.Class := if completed then notDoneButtonCss else doneButtonCss ]
            } $> state
