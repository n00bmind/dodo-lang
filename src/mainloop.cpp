
void Run( Array<String> const& argsList, MemoryArena* globalArena, MemoryArena* tmpArena )
{
    // Parse arguments
    if( argsList.count == 0 )
    {
        globalPlatform.Error( "Need an input file" );
        return;
    }

    String inputFilename = argsList[0];
    PlatformReadFileResult readResult = globalPlatform.ReadEntireFile( inputFilename.CString( tmpArena ), tmpArena );

    Parse( String( (char const*)readResult.contents, readResult.contentSize ) );
}
